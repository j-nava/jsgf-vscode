module VSCode.Extension

%export "javascript:test"
test : ()
test = unsafePerformIO $ do
  pure ()

main : IO ()
main = pure ()

