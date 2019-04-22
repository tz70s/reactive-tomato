import Reactive.Tomato
import Reactive.Tomato.Time

main :: IO ()
main = do
  putStrLn "Time operations profiling, focus on memory usage."
  timer0 <- every $ milli 10
  let sig0    = throttle timer0 $ generate [1 .. 1000]
  let sigfold = foldp (+) 0 sig0
  react sigfold print
