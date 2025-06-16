%%
%
% 6 Feb 2024
% Dr. Lowman
%
% The purpose of this code is use COVID-19 data to predict infection rates
% using deaths.
%
%%

clear all; % clear anything stored in command window
close all; % close any figures that are open

%% PART A
%% Read in COVID tracking data stored on my computer

% path to folder with data
pathDat = 'G:\My Drive\EGR_328\fall2022\labs\lab01';

% filename of data
filename = 'delaware-history.csv';

% open data and store into a matrix called 'A'
A = importdata(fullfile(pathDat,filename));

%% Visualize Data

% store current hospitalization to a vector
hospCurr = A.data(:,7);

% store dates to vector
dates = A.textdata(:,1);
dates(1) = []; % removes the 'date' text header

% plot hospitalizations versus time
% plot(dates,hospCurr);

% convert the "dates" vector from text data to numeric
date_str = string(dates);

% describe how the dates are formatted
formatIn = 'mm/dd/yyyy';

% convert the date_str to MATLAB'S internal datenum format
dates_num = datenum(date_str,formatIn);

% plot hospitalizations vs time
figure;
plot(dates_num,hospCurr);
datetick('x','dd-mmm-yy')

%% Inverse Model for Predicting the # of Infections per day

% store fatalities as a vector from my COVID dataset = data/observations
numFata = A.data(:,3);

% quant. model: mortality rate
mr = 9/712; % from Diamond Princess Cruiseship: 9 passed away/712 infected

% infer # of infected using my inverse problem
numInf = numFata/mr; 

% read in number of positive cases
numPos = A.data(:,20);

figure;
hold on;
plot(dates_num,numInf);
plot(dates_num,numPos);
datetick('x','dd-mmm-yy')
legend('# infected','# of positive tests')















