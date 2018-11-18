{-# OPTIONS -Weverything -fno-warn-implicit-prelude #-}
import Control.Concurrent
    ( ThreadId
    , forkIO
    , threadDelay
    )
import Control.Concurrent.Chan
    ( Chan
    , getChanContents
    , newChan
    , writeChan
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , putMVar
    , takeMVar
    )


main :: IO ()
main = do
    putStrLn "With 5 slow actions:"
    print =<< concurrently' 3 (replicate 5 slowAction)
    putStrLn "With no slow actions:"
    print =<< concurrently' 3 ([] :: [IO String])


-- | Some slow IO. This could be a file read, network request, or whatever.
slowAction :: IO String
slowAction = do
    threadDelay (2 * second)
    pure "Foo"
  where
    second :: Int
    second = 1000000 -- microseconds


-- | Run a bunch of actions concurrently, limiting the number of actions
-- executing at any one time.
--
-- This is useful for not exceeding open file limits, for example.
concurrently' :: Int -> [IO a] -> IO [a]
concurrently' _     [] = pure []
concurrently' limit io = do
    runningMVar <- newMVar 0      -- number of running threads
    resultChan  <- newChan        -- results channel
    throttle limit runningMVar resultChan io
    results <- take (length io) <$> getChanContents resultChan
    pure (forceSpine results)


throttle :: Int -> MVar Int -> Chan a -> [IO a] -> IO ()
throttle _     _           _          []                 = pure ()
throttle limit runningMVar resultChan (action : actions) = do
    runningThreads <- takeMVar runningMVar
    if runningThreads < limit
        then do
            putMVar runningMVar (runningThreads + 1)
            -- ignoring thread id
            _ <- runInThread
                (\a -> do
                    writeChan resultChan a
                    modifyMVar_ runningMVar (pure . subtract 1)
                )
                action
            throttle limit runningMVar resultChan actions
        else do
            putMVar runningMVar runningThreads
            throttle limit runningMVar resultChan (action : actions)


-- | Run an action in a new (lightweight) thread and pass the result to f.
runInThread :: (a -> IO ()) -> IO a -> IO ThreadId
runInThread f action = forkIO (action >>= f)


forceSpine :: [a] -> [a]
forceSpine as = foldr (const id) () as `seq` as
