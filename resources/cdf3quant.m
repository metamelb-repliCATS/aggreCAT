function EX_CDF = cdf3quant(q5,q50,q95)
%%function to write expert's cumulative distribution function when built from 3 quantiles q5,q50 and q95 
%%using the uniform background measure and the [0,100] support

%% this assumes the numbers are given as probabilities and transforms to 0,100 scale
%% COMMENT line 7 if the quantiles are actually already on the 0 to 100 scale
quant = [q5 q50 q95]*100;

%%
quant = round(sort(quant)); %% no more than 10^(-2) precision for the probabilities

quant = properise(quant);

q5 = quant(1);
q50 = quant(2);
q95 = quant(3);

  
%(0:q5*0.05/q5, ((q5+1):q50-q5)*0.45/(q50-q5)+0.05, ((q50+1):q95-q50)*0.45/(q95-q50)+0.5, ((q95+1):100-q95)*0.05/(100-q95)+0.95)
EX_CDF = [[0:q5].*(0.05/q5) ([(q5+1):q50]- q5).*(0.45/(q50-q5)) + ...
    0.05 ([(q50+1):q95]- q50).*(0.45/(q95-q50)) + ...
    0.5 ([(q95+1):100]-q95).*(0.05/(100-q95)) + 0.95];
end
