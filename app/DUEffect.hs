module DUEffect 
where
    
func a b = if b > 0 then c + 1 else c - 1
    where c = if a > 0 then 1 else -1   

--func' a = if a > 0 then 1 else -1 
--func a b = if b > 0 then (func' a) + 1 else (func' a) - 1
