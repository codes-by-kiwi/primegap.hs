isPrime :: Integer -> Bool --isPrime is a function that takes integer and returns Bool.
isPrime n = null [ x | x <- [2..n `div` 2], n `mod` x == 0]
--this function is used to tell us if a number is prime or not 

primes :: Integer -> Integer -> [Integer] -- it is a function that takes 2 integers and returns a list of integers 
primes a b = filter isPrime [a..b]
--this function is used to filter out the prime numbers between a and b 

gappies :: Integer -> Integer -> Integer -> [(Integer,Integer)] --this is a function that takes 3 integers and returns a list of tuples which has two integers each
gappies g a b = [(x,y)| x <- primes a b, y<-primes a b, y-x ==g] 
--this returns all the numbers between a and b which have a gap of g

twins = gappies 2
cousins = gappies 4
sexies = gappies 6
--the above are shortcuts that can be used instead of typing out the whole gappies function in the terminal 


-- essentially this code is done to figure out all the numbers between 2 numbers that have that gap and are prime.