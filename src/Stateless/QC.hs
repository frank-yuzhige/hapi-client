module Stateless.QC where
import Test.HAPI (QuickCheckConduct, quickCheckConduct, testM)
import Stateless.Example (cograph)

conduct :: QuickCheckConduct
conduct = quickCheckConduct cograph

main = testM conduct
