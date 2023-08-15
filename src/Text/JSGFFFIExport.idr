module Text.JSGFFFIExport

import FFI.FFIDerive
import Text.Bounded

%language ElabReflection

%runElab ffiexport "exportTypesTS" ["Bounds"] [] TypeScript

export
main : IO ()
main = putStrLn exportTypesTS
