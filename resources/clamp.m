% clamp x in [hi, lo]
function retval = clamp(x, lo, hi)
% Preconditions
if (hi < lo)
    error("lo must be smaller than or equal to hi");
end

if x < lo
    retval = lo;
elseif x > hi
    retval = hi;
else
    retval = x;
end