
-- | The type of Qubits in QIO are simply integer references.
newtype Qbit = Qbit Int deriving (Num, Enum, Eq, Ord)

-- | A rotation is in essence a two-by-two complex valued matrix
type Rotation = ((Bool,Bool) -> CC)

-- so far, I consider U to be Quantum gate, and QIO to be Quantum circuit.

-- | The underlying data type of a U unitary operation
data U = UReturn 
        | Rot Qbit Rotation U
        | Swap Qbit Qbit U 
        | Cond Qbit (Bool -> U) U 
        | Ulet Bool (Qbit -> U) U

-- U is a Monoid, which means it can form a semigroup, i.e. composing Quantum gates results in a new Quantum gate.
-- the important intuition here is that absorbing all Quantum gates into one, will resulting a huge block, which is actually a Quantum circuit. Doing so, the computation effect is already registered in the data type.
-- My opinion is that, according to (`Emily Riehl`)[https://emilyriehl.github.io/files/compose.pdf], computational effect should be managed using Monad, so why not treat U as monad?


-- | The type "U" forms a Monoid 
instance Monoid U where
    mempty = UReturn
    UReturn        <> u   = u
    (Rot x a u)    <> u'  = Rot x a (u <> u')
    (Swap x y u)   <> u'  = Swap x y (u <> u')
    (Cond x br u') <> u'' = Cond x br (u' <> u'')
    (Ulet b f u)   <> u'  = Ulet b f (u <> u')

-- some shortcut constructor for U.

-- | Apply the given rotation to the given qubit
rot :: Qbit -> Rotation -> U
rot x r = Rot x r UReturn

-- | Swap the state of the two given qubits
swap :: Qbit -> Qbit -> U
swap x y = Swap x y UReturn

-- | Apply the conditional unitary, depending on the value of the given qubit
cond :: Qbit -> (Bool -> U) -> U
cond x br = Cond x br UReturn

-- | Introduce an Ancilla qubit in the given state, for use in the sub-unitary
ulet :: Bool -> (Qbit -> U) -> U
ulet b ux = Ulet b ux UReturn

-- | Returns the inverse (or reverse) of the given unitary operation
urev :: U -> U
urev UReturn        = UReturn
urev (Rot x r u)    = urev u <> rot x (rrev r)
urev (Swap x y u)   = urev u <> swap x y
urev (Cond x br u)  = urev u <> cond x (urev.br)
urev (Ulet b xu u)  = urev u <> ulet b (urev.xu)

-- | Apply a not rotation to the given qubit
unot :: Qbit -> U
unot x = rot x rnot

-- | Apply a hadamard rotation to the given qubit
uhad :: Qbit -> U
uhad x = rot x rhad

-- | Apply a phase rotation (of the given angle) to the given qubit
uphase :: Qbit -> RR -> U
uphase x r = rot x (rphase r) 


-- What exactly are the differences between U and QIO?
-- remind that U : {UReturn, Rot, Swap, Cond, Ulet}
-- while QIO: {QReturn, MkQbit, ApplyU, Meas}
-- apparently, QIO contains all the side effect with respect to Qubit and U
-- Maybe for the mainstream circuit model, QIO is redundently powerful and complex. Maybe U is already enough for a circuit manipulating task.

-- | The underlying data type of a QIO Computation
data QIO a = QReturn a 
        | MkQbit Bool (Qbit -> QIO a) 
        | ApplyU U (QIO a) 
        | Meas Qbit (Bool -> QIO a)

instance Functor QIO where
    fmap = liftM
 
instance Applicative QIO  where
    pure  = QReturn
    (<*>) = ap

-- | The "QIO" type forms a Monad
instance Monad QIO where
    return = pure
    (QReturn a) >>= f = f a
    (MkQbit b g) >>= f = MkQbit b (\ x -> g x >>= f)
    (ApplyU u q) >>= f = ApplyU u (q >>= f)
    (Meas x g) >>= f = Meas x (\ b -> g b >>= f)

-- I call this shortcut function

-- | Initialise a qubit in the given state (adding it to the overall quantum state)
mkQbit :: Bool -> QIO Qbit
mkQbit b = MkQbit b return

-- | Apply the given unitary operation to the current quantum state
applyU :: U -> QIO ()
applyU u =  ApplyU u (return ())

-- | Measure the given qubit, and return the measurement outcome (note that this
-- operation may affect the overall quantum state, as a measurement is destructive)
measQbit :: Qbit -> QIO Bool
measQbit x = Meas x return



-- rotation utility

-- | The identity rotation
rid :: Rotation
rid (x,y) = if x==y then 1 else 0

-- | The not rotation
rnot :: Rotation
rnot (x,y) = if x==y then 0 else 1

-- | The hadamard rotation
rhad :: Rotation
rhad (x,y) = if x && y then -h else h where h = (1/sqrt 2)

-- | The phase rotation
rphase :: RR -> Rotation
rphase _ (False,False)  = 1
rphase r (True,True)    = exp(0:+r)
rphase _ (_,_)          = 0

-- | Returns the inverse (or reverse) of the given rotation
rrev :: Rotation -> Rotation
rrev r (False,True)   = conjugate (r (True,False)) 
rrev r (True,False)   = conjugate (r (False,True))
rrev r xy             = conjugate (r xy)

-- | Rotations can be compared for equality.
-- They are equal if the define the same matrix.
instance Eq Rotation where
    f == g =    (f (False,False)  == g (False,False)) 
             && (f (False,True)   == g (False,True)) 
             && (f (True,False)   == g (True,False)) 
             && (f (True,True)    == g (True,True))
    f /= g =    (f (False,False)  /= g (False,False)) 
             || (f (False,True)   /= g (False,True)) 
             || (f (True,False)   /= g (True,False)) 
             || (f (True,True)    /= g (True,True))