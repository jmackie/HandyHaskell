import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem
import Control.Exception (bracket_)

speedyTraverse
  :: Traversable t
  => Maybe Int   -- max jobs (-j)
  -> (a -> IO b)
  -> t a
  -> IO (t b)
speedyTraverse Nothing f jobs = mapConcurrently f jobs
speedyTraverse (Just j) f jobs = do
  sem <- newQSem j
  mapConcurrently (bracket_ (waitQSem sem) (signalQSem sem) . f) jobs
