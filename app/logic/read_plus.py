import os
import shutil
import sys
import re
import zipfile
import pandas as pd
from zipfile import ZipFile
from fastavro import reader

def get_timestamp_column(start_time,sampling_freq,len_list):
    start_time_ns = start_time * 1000
    start_timestamp = pd.to_datetime(start_time_ns, unit='ns')

    # Calculate end_timestamp based on the length of the list and sampling frequency
    end_timestamp = start_timestamp + pd.to_timedelta(len_list / sampling_freq, unit='s')

    # Generate a range of timestamps from start to end with the given frequency
    timestamp_column = pd.date_range(start=start_timestamp, end=end_timestamp, freq=pd.to_timedelta(1 / sampling_freq, unit='s'))
    timestamp_df = pd.DataFrame({'timestamp': timestamp_column})
    
    # Convert 'timestamp' column back to Unix timestamp in seconds
    timestamp_df['unix_timestamp'] = timestamp_df['timestamp'].astype('int64') // 10**9
    return timestamp_df

def get_avro_content(zip_file_path, avro_file_path_within_zip, extracted_folder):
    
    # Extract the contents of the zip file
    with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
        zip_ref.extractall(extracted_folder)
    
    # Construct the full path to the Avro file after extraction
    avro_file_path = os.path.join(extracted_folder, avro_file_path_within_zip)
    
    # Read the Avro file and convert it to a DataFrame
    avro_records = []
    with open(avro_file_path, 'rb') as avro_file:
        avro_reader = reader(avro_file)
        for record in avro_reader:
            
            acc_start=record['rawData']['accelerometer']['timestampStart']
            acc_sampling_freq=record['rawData']['accelerometer']['samplingFrequency']
            
            acc_x=record['rawData']['accelerometer']['x']
            acc_y=record['rawData']['accelerometer']['y']
            acc_z=record['rawData']['accelerometer']['z']
            
            acc_x_df = pd.DataFrame({'x':acc_x})
            acc_y_df = pd.DataFrame({'y':acc_y})
            acc_z_df = pd.DataFrame({'z':acc_z})
            
            if acc_x_df.empty or acc_y_df.empty or acc_z_df.empty:
                print('Accelerometer empty for:', avro_file_path)
                acc_df=pd.DataFrame()
            else:
                timestamp_df=get_timestamp_column(acc_start,acc_sampling_freq,len_list=len(acc_x))
                acc_df= pd.concat([acc_x_df, acc_y_df, acc_z_df, timestamp_df], axis=1)
            
            
            gy_start=record['rawData']['gyroscope']['timestampStart']
            gy_sampling_freq=record['rawData']['gyroscope']['samplingFrequency']
            gy_x=record['rawData']['gyroscope']['x']
            gy_y=record['rawData']['gyroscope']['y']
            gy_z=record['rawData']['gyroscope']['z']
            
            gy_x_df = pd.DataFrame({'x':gy_x})
            gy_y_df = pd.DataFrame({'y':gy_y})
            gy_z_df = pd.DataFrame({'z':gy_z})
            
            if gy_x_df.empty or gy_y_df.empty or gy_z_df.empty:
                print('Gyroscope empty for:', avro_file_path)
                gy_df=pd.DataFrame()
            else:
                timestamp_df=get_timestamp_column(gy_start,gy_sampling_freq,len_list=len(gy_x))
                gy_df= pd.concat([gy_x_df, gy_y_df, gy_z_df, timestamp_df], axis=1)
              
            
            eda_start=record['rawData']['eda']['timestampStart']
            eda_sampling_freq=record['rawData']['eda']['samplingFrequency']
            eda=record['rawData']['eda']['values']
            
            eda_df = pd.DataFrame({'eda':eda})
            if eda_df.empty :
                print('EDA empty for:', avro_file_path)
                eda_df=pd.DataFrame()
            else:
                timestamp_df=get_timestamp_column(eda_start,eda_sampling_freq,len_list=len(eda))
                eda_df= pd.concat([eda_df, timestamp_df], axis=1)
            
            temp_start=record['rawData']['temperature']['timestampStart']
            temp_sampling_freq=record['rawData']['temperature']['samplingFrequency']
            temp=record['rawData']['temperature']['values']
            
            temp_df = pd.DataFrame({'temp':temp})
            if temp_df.empty :
                print('Temperature empty for:', avro_file_path)
                temp_df=pd.DataFrame()
            else:
                timestamp_df=get_timestamp_column(temp_start,temp_sampling_freq,len_list=len(temp))
                temp_df= pd.concat([temp_df, timestamp_df], axis=1)
            
            bvp_start=record['rawData']['bvp']['timestampStart']
            bvp_sampling_freq=record['rawData']['bvp']['samplingFrequency']
            bvp=record['rawData']['bvp']['values']
            
            bvp_df = pd.DataFrame({'bvp':bvp})
            if bvp_df.empty :
                print('BVP empty for:', avro_file_path)
                bvp_df=pd.DataFrame()
            else:
                timestamp_df=get_timestamp_column(bvp_start,bvp_sampling_freq,len_list=len(bvp))
                bvp_df= pd.concat([bvp_df, timestamp_df], axis=1)
            
            steps_start=record['rawData']['steps']['timestampStart']
            steps_sampling_freq=record['rawData']['steps']['samplingFrequency']
            steps=record['rawData']['steps']['values']
            
            steps_df = pd.DataFrame({'steps':steps})
            if steps_df.empty :
                print('Steps empty for:', avro_file_path)
                steps_df=pd.DataFrame()
            else:
                timestamp_df=get_timestamp_column(steps_start,steps_sampling_freq,len_list=len(steps))
                steps_df= pd.concat([steps_df, timestamp_df], axis=1)
            
            systolic_peaks=record['rawData']['systolicPeaks']['peaksTimeNanos']
            
            systolic_peaks_df = pd.DataFrame({'systolic_peaks':systolic_peaks})
            
            avro_dicts={}
            avro_dicts['ACC']=acc_df
            avro_dicts['GY']=gy_df
            avro_dicts['EDA']=eda_df
            avro_dicts['TEMP']=temp_df
            avro_dicts['BVP']=bvp_df
            avro_dicts['steps']=steps_df
            avro_dicts['systolic_peaks']=systolic_peaks_df
            
            avro_records+=[avro_dicts]
    try:
        shutil.rmtree(extracted_folder)
        print(f"The folder '{extracted_folder}' and its contents have been successfully removed.")
    except OSError as e:
        print(f"Error: {e}")
        
    return avro_records

def read_e4_plus(root, extracted_folder):
    
    avro_content=[]   
    zf = ZipFile(root)
    file_names = zf.namelist()
    
    avro_file_names = [file_name for file_name in file_names if file_name.lower().endswith(".avro")]
    
    for file in avro_file_names:    
        avro_content+=get_avro_content(root, file, extracted_folder)
            
    return avro_content

print('Python Script Loaded Successfully')
