
% order v so that its 3 elements are distinct, fall inside [1, 99] and are ordered 
function retval = properise(v) 
    % Preconditions
    if (size(v) ~= 3)
        error("properise_quantiles: v expected to have 3 elements");
    end    
    v = sort(v);
    
    v(2) = clamp(v(2), 2, 98);
    v(1) = clamp(v(1), 1, v(2) - 1);
    v(3) = clamp(v(3), v(2) + 1, 99);    
    
    retval = v;
