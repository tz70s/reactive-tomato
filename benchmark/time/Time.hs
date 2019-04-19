import Reactive.Tomato

main :: IO ()
main = do
  putStrLn "Time operations profiling, focus on memory usage."
  timer0 <- every $ milli 10
  let sig0    = throttle timer0 $ listGen [1 .. 1000]
  let sigfold = foldp (+) 0 sig0
  react sigfold print
