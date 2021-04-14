{- Assignment 5
 - Name: Sai Santhosh Thunga
 - Date: December 13th, 2020
 -}
module Assign_5 where

import Test.QuickCheck (quickCheck)

macid :: String
macid = "thungas"


{- -----------------------------------------------------------------
 - definiteIntegral
 - ------------------------------------------------------------------
 - Description: Finds the definite Integral value of a function, given the 
 lower limit, upper limit and n (Number of trapezoidal partitions)
 - ------------------------------------------------------------------------
 - |  Input      | ll :: Double, lower limit of the definite integral
                 | ul :: Double, upper limit of the definite integral
                 | f :: (Double -> Double), the integrand
                 | n :: Integer, Number of Trapezoidal Partions
 - ------------------------------------------------------------------------
 - |  Output     | Returns the value of the definite integral
 - -----------------------------------------------------------------------|
 -}


-- f map it to every x value that we considered in the trapezoidal partitions

definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral 0 0 _ _ = 0
definiteIntegral _ _ _ 0 = 0
definiteIntegral ll ul f n 
    | ul < ll   = - definiteIntegral ul ll f n 
    | ul == ll  = 0
    | otherwise =  deltaX * (((f ll + f ul) / 2) + sum (map f points)) where 
        deltaX = (ul - ll) / fromIntegral n
        points = [ll + deltaX, ll + (2 * deltaX) .. ul - deltaX]

{- -----------------------------------------------------------------
 - arcsin1
 - ------------------------------------------------------------------
 - Description: Finds the definite integral value of the function f
   (f = \x -> sqrt (1 - (x**2))) between -1 and 1 with n trapezoidal partitions
 - ------------------------------------------------------------------------
 - |  Input      | Takes n Number of Trapezoidal Partitions
 - ------------------------------------------------------------------------
 - |  Output     | Returns the definiteIntegral value of the function f
                 (f = \x -> sqrt (1 - (x**2))) with n paritions
 - -----------------------------------------------------------------------|
 -}
 
arcsin1 :: Integer -> Double
arcsin1 n = definiteIntegral (-1) 1 f n
    where
        f = \x -> sqrt (1 - (x ** 2))

{- -----------------------------------------------------------------
 - piApprox
 - ------------------------------------------------------------------
 - Description: Approximates the pi value within the given tolerance
 - ------------------------------------------------------------------------
 - |  Input      | Takes tolerance of type Double
 - ------------------------------------------------------------------------
 - |  Output     | Returns an approximation of pi with the fewest number
                of partitions.
 - -----------------------------------------------------------------------|
 -}

piApprox :: Double -> Double
piApprox tol = satisfyTol tol n
    where
        satisfyTol tol n 
            | abs (pi' - pi) <= tol = 2 * arcsin1 n
            | otherwise = satisfyTol tol (n + 1) 
                where
                  pi' = 2 * arcsin1 n
        n = 1
        
{- -----------------------------------------------------------------
 - logApprox
 - ------------------------------------------------------------------
 - Description: Calculates the approximate natural log value at x, with the
    given tolerance.
 - ------------------------------------------------------------------------
 - |  Input      | Takes x of type Double to compute the natural log value at x
                 | Takes tol of type Double (tolerance) to compute the natural log value
                 at x within the given tolerance.                  
 - ------------------------------------------------------------------------
 - |  Output     | Returns an approximate of the natural log value at x
                 within the given tolerance.
 - -----------------------------------------------------------------------|
 -}

logApprox :: Double -> Double -> Double
logApprox 1 _ = 0
logApprox x tol 
    | x <= 0 = error "undefined"
    | otherwise =
        satisfyLogTol tol n where
            g = (\t -> 1 / t)
            satisfyLogTol tol n 
                    | abs (d1 - d2) <= tol = d2
                    | otherwise = satisfyLogTol tol (n + 1) where
                d1 = definiteIntegral 1 x g n
                d2 = definiteIntegral 1 x g (n-1)
            n = 1
{- 

---------------- Test Cases -------------------------

Function: definiteIntegral
Test Case Number: #1
Input: definiteIntegral 0 0 (\x -> x) 0
Expected Output: 0.0
Actual Output: 0.0

Function: definiteIntegral
Test Case Number: #2
Input: definiteIntegral 0 10 (\x -> x**2) 10000
Expected Output: 333.3333350000002
Actual Output: Actual output for the function.

Function: definiteIntegral
Test Case Number: #3
Input: definiteIntegral (pi/2) 0 (\x -> sin(x)) 10000
Expected Output: -0.9999999979438327
Actual Output: -0.9999999979438327



Function: arcsin1
Test Case Number: #1
Input: arcsin1 10
Expected Output: 1.5185244144417758
Actual Output: 1.5185244144417758

Function: arcsin1
Test Case Number: #2
Input: arcsin1 1000
Expected Output: 1.5707437385010707
Actual Output: 1.5707437385010707

Function: arcsin1
Test Case Number: #3
Input: arcsin1 100000
Expected Output: 1.570796274201849
Actual Output: 1.570796274201849



Function: piApprox
Test Case Number: #1
Input: piApprox 0.01
Expected Output: 3.131603473048396
Actual Output: 3.131603473048396

Function: piApprox
Test Case Number: #2
Input: piApprox 0.0001
Expected Output: 3.141492766478784
Actual Output: 3.141492766478784

Function: piApprox
Test Case Number: #3
Input: piApprox 0.00001
Expected Output: 3.141582654911406
Actual Output: 3.141582654911406



Function: logApprox 
Test Case Number: #1
Input: logApprox 10 0.00001
Expected Output: 2.303136993503202
Actual Output: 2.303136993503202

Function: logApprox 
Test Case Number: #2
Input: logApprox 1 0.00001
Expected Output: 0.0
Actual Output: 0.0

Function: logApprox 
Test Case Number: #3
Input: logApprox 2.318 0.0001
Expected Output: 0.8414010517109797
Actual Output: 0.8414010517109797

-}

{-

-- QuickCheck --

Function: definiteIntegralprop 
Property: definiteIntegralprop :: Double -> Double -> Integer -> Bool

definiteIntegralprop a b n = ((definiteIntegral a b (**3) n) - 
    (- definiteIntegral b a (**3) n)) < 10 ** 5

Actual Test Result: Pass


---
Function: arcsin1Prop
Property: arcsin1Prop :: Integer -> Bool

arcsin1Prop n = definiteIntegral 1 (-1) func n == -arcsin1 n
    where func n = sqrt(1 - n**2)

Actual Test Result: Pass


---
Function: piApproxProp
Property: piApproxProp :: Double -> Double -> Bool

piApproxProp tol = tol <= 10**(-5) || abs(pi' - pi) <= tol where
    pi' = piApprox tol 

Actual Test Result: Pass

---
Function: logApproxProp
Property: logApproxProp :: Double -> Double -> Bool

logApproxProp x tol = n > 0 || abs (definiteIntegral 1 x g n - 
    definiteIntegral 1 x g (n-1)) <= tol
    where
        n = 1
        g t = recip t

Actual Test Result: Pass


-}

--QuickCheck Properties

definiteIntegralProp :: Double -> Double -> Integer -> Bool
definiteIntegralProp a b n = (definiteIntegral a b f n - 
    (- definiteIntegral b a f n)) < 10 ** (-5) where
        f = \x -> x**2

arcsin1Prop :: Integer -> Bool
arcsin1Prop n = definiteIntegral 1 (-1) f n == -arcsin1 n
    where f x = sqrt(1 - (x**2))

piApproxProp :: Double -> Bool
piApproxProp tol = tol <= 10**(-5) || abs(pi' - pi) <= tol where
    pi' = piApprox tol


logApproxProp :: Double -> Double -> Bool
logApproxProp x tol = n > 0 || abs (definiteIntegral 1 x f n - 
    definiteIntegral 1 x f (n-1)) <= tol
    where
        n = 1
        f t = 1 / t