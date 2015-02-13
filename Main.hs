-- |

module Main where
import qualified Control.Monad.State as ST
data Closure i o = Respond (i -> (o, Closure i o))
-- defines a type that at each step (invocation)
-- takes a value of type i and is used to compute a pair of type o
incrementer :: Closure () Int
incrementer = go n where
  n = 0
  go n' = Respond $ \_ -> (n', go (n' + 1))


query :: i -> Closure i o -> (o , Closure i o)
query i (Respond f) = f i -- f is  closure

query'    :: i -> ST.State (Closure i o) o
query' i  = ST.state $ \(Respond f) -> f i

someQuery :: ST.State (Closure () Int) (Int, Int)
someQuery = do
  n1 <- query' ()
  n2 <- query' ()
  return (n1,n2)
-- defines a newtype State, with one constructor State
-- state is defined in record Syntax
-- runState is a closure that takes a state and returns
-- a new accumulator and a new state.
newtype State s a = State { runState :: s -> (a, s) }
