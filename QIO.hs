
-- | The type of Qubits in QIO are simply integer references.
newtype Qbit = Qbit Int deriving (Num, Enum, Eq, Ord)

-- | A rotation is in essence a two-by-two complex valued matrix
type Rotation = ((Bool,Bool) -> CC)

-- so far, I consider `U` to be Quantum gate as well as Quantum Circuit, and `QIO` to be all the side effect which can happen on a Quantum gate/circuit.

-- note that Rotation contains two much information of a QGate, i.e. the unitary matrix. I believe this is unneccessary, since only numerical simulator would need such information, real hardware or symbolic optimizer do not need such a thing. I think put Rotation in such a low level(or high level if you think so) is not a good design.

-- the better design, I suppose, should be that Rotation is any thing belonging to the Monoid typeclass. 

-- | The underlying data type `U` of a unitary operation
data U = UReturn 
        | Rot Qbit Rotation U
        | Swap Qbit Qbit U 
        | Cond Qbit (Bool -> U) U 
        | Ulet Bool (Qbit -> U) U

-- `U` is a Monoid, which means it can form a semigroup, i.e. composing Quantum gates results in a new Quantum gate.
-- the important intuition here is that absorbing all Quantum gates into one, will resulting a huge block, which is actually a Quantum circuit. Doing so, the computation effect is already registered in the data type.
-- My opinion is that, according to [`Emily Riehl`](https://emilyriehl.github.io/files/compose.pdf), computational effect should be managed using Monad, so why not treat `U` as monad?

-- The QGate and circuit are just the same thing, I believe this is a very good design! and is what makes functional programming show its full power

-- mainstream python library seperate QGate and Circuit into different classes, where QGate only handle one or two Qubits, and Circuits are string/list of QGate. This brings a lot of problems, and I believe, because of this, many python libary (such as qiskit) use a recursive definition of QGate and Circuit(QGate is defined using function calling Circuit(if neccessary) and Circuit is defined using function calling QGate, if neccessary), taking advantages of the dynamic feature of python languages. Others(pyzx for example) quietly transform a QGate into Circuit and vice verse when neccessary, even under operators that should be Monoid(for example, + * operator). That is really a disaster.

-- | The type `U` forms a Monoid 
instance Monoid U where
    mempty = UReturn
    UReturn        <> u   = u
    (Rot x a u)    <> u'  = Rot x a (u <> u')
    (Swap x y u)   <> u'  = Swap x y (u <> u')
    (Cond x br u') <> u'' = Cond x br (u' <> u'')
    (Ulet b f u)   <> u'  = Ulet b f (u <> u')

-- some shortcut constructor for `U`.

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

-- pay attention to the bind function of MkQbit, it just register another function inside the function slot of the constructor 

-- | The "QIO" type forms a Monad
instance Monad QIO where
    return = pure
    (QReturn a) >>= f = f a
    (MkQbit b g) >>= f = MkQbit b (\ x -> g x >>= f)
    (ApplyU u q) >>= f = ApplyU u (q >>= f)
    (Meas x g) >>= f = Meas x (\ b -> g b >>= f)

-- I call this shortcut function


-- if we have, for example 
--     q0 :: QIO Qbit
--     q0 = mkQbit False
-- then 
--     MkQbit Bool (Qbit -> QIO a) 
-- will change to 
--     MkQbit Bool (Qbit -> QIO Qbit)
-- note that nothing special happened here
-- where (Qbit -> QIO Qbit) is just a trivial thin layer 
-- In this case, you choose to do nothing. Note that there could also be some other thing happened here, i.e. you can choose functions other than `return` 

-- all about ADT is registering
-- what really matter is what happened in the trigger function, which should have the signature of 
--      QIO a -> a 


-- | Initialise a qubit in the given state (adding it to the overall quantum state)
mkQbit :: Bool -> QIO Qbit
mkQbit b = MkQbit b return

-- again, apply just called the ApplyU constructor, which do nothing but registering a QGate `U` inside.

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



-- | The 'Qdata' type class defines the operation a quantum datatype must implement.
class Qdata a qa | a -> qa, qa -> a where
    mkQ :: a -> QIO qa
    measQ :: qa -> QIO a
    letU :: a -> (qa -> U) -> U
    condQ :: qa -> (a -> U) -> U


-- remind that `Qbit` is just a data type with one constructor: `Qbit Int`
-- and mkQbit has the type signature of `mkQbit :: Bool -> QIO Qbit`

-- | The lowest-level instance of Qdata is the relation between Booleans and Qubits.
instance Qdata Bool Qbit where
    mkQ = mkQbit
    measQ = measQbit
    letU b xu = ulet b xu
    condQ q br = cond q br

-- examples:



-- | Initialise a qubit in the |0> state
q0 :: QIO Qbit
q0 = mkQ False

-- qPlus firstly initialize a `QIO Qbit` object, and then it registers `ApplyU` into its function slots
-- note that the signature of uhat is 
--      uhad :: Qbit -> U
-- which means, after this bind function, the signature of 
--      QIO Qbit 
-- will becomes 
--      QIO U

-- | Initialise a qubit in the |+> state. This is done by applying a Hadamard
-- gate to the |0> state.    
qPlus :: QIO Qbit
qPlus  =  do  qa <- q0
              applyU (uhad qa)
              return qa

-- q0 : MkQbit b QReturn
-- QReturn :: a -> QIO a 
-- (MkQbit b g) >>= f = MkQbit b (\ x -> g x >>= f)
-- replace g with QReturn
-- \x -> QReturn x >>= f 

-- consider the bind rule of QReturn
-- (QReturn a) >>= f = f a
-- apply
-- \qa -> applyU (uhad qa) 

-- checkpoint: MkQbit b (\qa -> applyU (uhad qa) )

-- applyU u =  ApplyU u (return ())

-- replace u with uhad qa 
-- ApplyU (uhad qa) (return ())

-- uhad :: Qbit -> U
-- uhad x = rot x rhad

-- checkpoint: MkQbit b (\qa -> ApplyU (rot qa rhad) (return ()) )

-- again: (MkQbit b g) >>= f = MkQbit b (\ x -> g x >>= f)

-- \qa -> (ApplyU (rot qa rhad) (return ())) >>= QReturn

-- again: (ApplyU u q) >>= f = ApplyU u (q >>= f)

-- \qa -> (ApplyU (rot qa rhad) (QReturn qa))

-- checkpoint: MkQbit b  (\qa -> (ApplyU (rot qa rhad) (QReturn qa)))