pricePerHour :: String -> Float
pricePerHour "Kseniia" = 15 * oneUSD
pricePerHour "Ilya" = 15 * oneUSD
pricePerHour _ = 15 * oneUSD

usd2hkd :: Float -> Float
usd2hkd = (*oneUSD)

hkd2usd :: Float -> Float
hkd2usd = (/oneUSD)

oneUSD :: Float
oneUSD = 7.7799

oneEUR :: Float
oneEUR = 8.21137

printTable :: (Show a) => [(String, a)] -> IO ()
printTable = mapM_ (\ (str, a) -> putStrLn ("  " ++ str ++ ": " ++ show a))

printMoney :: String -> Float -> IO ()
printMoney msg amt = do
  putStrLn msg
  putStrLn $ show amt ++ " HKD = " ++ show (amt / oneUSD) ++ " USD."

transferredHKD :: Float
transferredHKD = 500.0 + 200.0 * oneEUR + 157.26 * oneUSD + 190.0 * oneUSD

givenHKD :: Float
givenHKD = 100.0 * oneUSD + 90.0 * oneUSD

haveNowHKD :: Float
-- haveNowHKD = 500.0 + 200.0 * oneEUR + 157.26 * oneUSD - 100.0 * oneUSD + 190.0 * oneUSD - 90.0 * oneUSD
haveNowHKD = transferredHKD - givenHKD

main :: IO ()
main = do
  -- list of tutoring sessions, (Name, Date Hours)
  let list :: [(String, [Float])]
      list = [
          (
            "Kseniia",
            [
              2.0, -- 2024-10-20, 10:00-12:00 
              2.0, -- 2024-10-21, 09:30-11:30 
              1.0, -- 2024-10-23, 21:00-22:30
              2.0, -- 2024-10-25, 10:30-12:30 
              2.0, -- 2024-10-26, 10:00-12:00 
              1.0, -- 2024-11-02, 19:30-12:30 
              1.0, -- 2024-11-09, 10:00-11:00 
              1.0, -- 2024-11-10, 16:00-17:00 
              1.0, -- 2024-11-17, 11:00-12:00 
              1.0, -- 2024-11-23, 10:00-11:00 
              1.0, -- 2024-11-30, 11:00-12:00 
              1.0, -- 2024-12-01, 11:00-12:00 
              1.0, -- 2024-12-07, 10:00-11:00 
              1.0  -- 2024-12-10, 10:00-11:00 
            ]
          ),
          (
            "Ilya",
            [
              1.0, -- 2024-10-20, 21:00-22:00
              1.0, -- 2024-10-27, 21:00-22:00
              1.0, -- 2024-11-17, 16:00-17:00 
              1.0, -- 2024-12-01, 21:00-22:00 
              1.5, -- 2024-12-25, 14:00-15:30 
              1.5, -- 2025-01-04, 14:00-15:30 
              1.0  -- 2025-01-12, 14:00-15:00 
            ]
          )
        ]
      totalHours = [(str, sum record) | (str, record) <- list]
      totalMoneyHKD = sum [hrs * pricePerHour str | (str, hrs) <- totalHours]
  putStrLn "Total hours:"
  printTable totalHours

  printMoney "Payroll:" totalMoneyHKD
  printMoney "Received:" transferredHKD
  printMoney "Given:" givenHKD
  printMoney "Balance:" (haveNowHKD - totalMoneyHKD)
