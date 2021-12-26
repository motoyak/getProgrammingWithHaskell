harmonic 0 = 0
harmonic n = foldl (+) 0 (map (\x -> 1 / x) [1..n])

harmonic' n = sum (take n seriesValues)
              where seriesPairs = zip (cycle [1.0]) [1.0,2.0 .. ]
                    seriesValues = map
                                  (\pair -> (fst pair)/(snd pair))
                                  seriesPairs

harmonic'' n = sum (take n numberSeq)
              where numberSeq = map (1.0/) (cycle [1.0, 2.0 ..])