-- A very minimal unit test runner.  A quick way to do TDD in replit
module MiniTest where

shouldBe :: Show a => a -> a -> IO()
shouldBe expr expected =
  if show expr == show expected then
    putStrLn $ "Passed " ++ show expected
  else do
    putStrLn $ "Failed " ++ show expected
    putStrLn $ "   got " ++ show expr
    error $ "Failed " ++ show expected
