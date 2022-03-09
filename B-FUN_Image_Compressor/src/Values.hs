module Values where

data Options = Options {
    colors :: Int,
    limit :: Float,
    file :: String
}

data Pixel = Pixel {
    position :: [Int],
    color :: [Int]
}

data Cluster = Cluster {
    clusterColor :: [Int],
    points :: [Pixel]
}

usage :: String
usage = "USAGE: ./imageCompressor -n N -l L -f F\n\n\
        \\tN\tnumber of colors in the final image\n\
        \\tL\tconvergence limit\n\
        \\tF\tpath to the file containing the colors of the pixels"