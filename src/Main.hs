import qualified HS2048
import Data.Monoid

-- LHC monoid modelled after http://milrevel.github.io/LHC
data ElementaryParticle = None
                         | Electron
                         | ElectronNeutrino
                         | Muon
                         | MuonNeutrino
                         | Tau
                         | TauNeutrino
                         | Gluon
                         | Photon
                         | ZBoson
                         | WBoson
                         | HiggsBoson
                         deriving (Show, Eq)

instance Monoid ElementaryParticle where
    mappend Electron Electron = ElectronNeutrino
    mappend ElectronNeutrino ElectronNeutrino = Muon
    mappend Muon Muon = MuonNeutrino
    mappend MuonNeutrino MuonNeutrino = Tau
    mappend Tau Tau = TauNeutrino
    mappend TauNeutrino TauNeutrino = Gluon
    mappend Gluon Gluon = Photon
    mappend Photon Photon = ZBoson
    mappend ZBoson ZBoson = WBoson
    mappend WBoson WBoson = HiggsBoson
    mappend a _ = a
    mempty = None

mainLHC = HS2048.main Electron
mainSum = HS2048.main (Sum 2) -- The original 2048
mainProduct = HS2048.main (Product 2)
mainListConcat = HS2048.main [42]
mainStringConcat = HS2048.main "!"

main :: IO ()
main = mainLHC
