clear;clc

%% the data has no column names, it is just the numbers, but the columns are as below
%% [1          2            3           4           5          6          7             8          9     ]
%%[userId	questionId	round1High	round1Middle round1Low	round2High round2Middle round2Low	outcome  ]
load data

questions = unique(data(:,2)); % the questions answered by numerical ID
no_quest = length(questions); % number of unique questions

exp = unique(data(:,1)); % experts by nemrical ID
no_exp = length(exp); %  number of unique experts
 

for i=1:no_quest
    temp =[];
    temp = data(find(data(:,2)== questions(i)),:);
    s(i) = size(temp,1);
    EW = 0; %% the Equally Weighted (EW) combination of distributions
    for j=1:s(i)
        f(j,:) = cdf3quant(temp(j,8),temp(j,7),temp(j,6));
        EW = EW + 1/s(i).*f(j,:);
    end
    EWDM(i,:)= [questions(i) EW temp(1,9)];  %% EW "decision maker" ' s distribution
    
end

supp = [0:100]'; %% support for the variable
for i=1:no_quest
    temp = [];
    EWDM_data_q(i,1) = EWDM(i,1);
    temp = [supp EWDM(i,2:(end-1))'];
    EWDM_Q(i,2) = interp1(temp(:,2),temp(:,1),0.05,'nearest');
    EWDM_q(i,3) = interp1(temp(:,2),temp(:,1),0.5,'nearest');
    EWDM_q(i,4) = interp1(temp(:,2),temp(:,1),0.95,'nearest');
    EWDM_q(i,5) = EWDM(i,end);
end