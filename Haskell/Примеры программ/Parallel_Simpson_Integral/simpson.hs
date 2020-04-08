import Data.Time
import Control.Parallel.Strategies

recurseSum :: Int -> Int -> Double -> Double -> (Double -> Double) -> Double
recurseSum k kMax startPoint addition intFunc = sum [intFunc x | x <- take pointsCnt points]
    where   pointsCnt = kMax - k
            points = [startPoint,nextPoint..]
            nextPoint = startPoint + addition

calcIntegralParallel :: Double -> Double -> Double -> Int -> Int -> (Double -> Double) -> Double
calcIntegralParallel prevInt a h m parCnt intFunc   
    | parCnt > 1 = (prevInt / 2.0) + ((h / (fromIntegral m)) * intValPar)
    | otherwise = (prevInt / 2.0) + ((h / (fromIntegral m)) * intValCons)
        where   k = 0
                kMax = div m 2                                                  
                addition = 2 * h / (fromIntegral m) 
                startPoint = a + (h / (fromIntegral m))
                intValCons = recurseSum k kMax startPoint addition intFunc
                intValPar = sum ([intSum x | x <- [0..parCnt-1]] `using` parList rdeepseq)
                    where   kStep = div kMax parCnt
                            lastKStep = kStep * (parCnt-1)                           
                            intSum x
                                | x < parCnt-1 = recurseSum k kStep parStartPoint addition intFunc
                                | otherwise = recurseSum lastKStep kMax parStartPoint addition intFunc
                                where   parStartPoint = startPoint + ((fromIntegral kStep) * addition * (fromIntegral x))

calcFirstIntegral :: Double -> Double -> Double -> (Double -> Double) -> Double
calcFirstIntegral h a b intFunc = (h / 2.0) * ((intFunc a) + (intFunc b))

rungeCheck :: Double -> Double -> Double -> Bool
rungeCheck prevInt curInt eps   
    | ((abs(prevInt - curInt)) / 3.0) > eps = False -- continue calculation
    | otherwise = True --return cur            

recurseCalc :: Double -> Double -> Double -> Double -> Int -> Int -> Double -> (Double -> Double) -> Double  
recurseCalc prevInt curInt a h m parCnt eps intFunc 
    | runge = curInt
    | otherwise = recurseCalc curInt nextInt a h (m * 2) parCnt eps intFunc
        where   runge = rungeCheck prevInt curInt eps
                nextInt = calcIntegralParallel curInt a h (m * 2) parCnt intFunc
    
calc :: Double -> Double -> Double -> Int -> (Double -> Double) -> Double
calc a b eps parCnt intFunc = recurseCalc firstInt secondInt a h m parCnt eps intFunc
    where   m = 2
            h = b - a
            firstInt = calcFirstIntegral h a b intFunc
            secondInt = calcIntegralParallel firstInt a h m parCnt intFunc
            
f1 :: Double -> Double
f1 x = (x * x + 1) * cos(0.5 * x)

f2 :: Double -> Double
f2 x = (x*x + 3*x + 11) / (x*x + 11*x - 7)

f3 :: Double -> Double
f3 x = sin(0.5*x*x*x)*sin(0.25*x*x)*sin(0.125*x)
                                                            
main = do
    start <- getCurrentTime
    print (calc 0.0 4000.0 0.00001 8 f1)
    end <- getCurrentTime
    print (diffUTCTime end start)
    start <- getCurrentTime
    print (calc 1.0 10000.0 0.0000001 8 f2)
    end <- getCurrentTime
    print (diffUTCTime end start)
    start <- getCurrentTime
    print (calc 0.0 100.0 0.0000001 8 f3)
    end <- getCurrentTime
    print (diffUTCTime end start)