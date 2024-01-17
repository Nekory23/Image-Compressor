# Image-Compressor
{EPITECH} | Second year | Functionnal programming

A pretty basic way to compress image consists in reducing the number of colors it contains.

3 steps are needed to do so:
1. read the image and extract the colors of each pixel,
2. cluster these colors, and replace each color of a given cluster by the mean color of this cluster,
3. index the means of the cluster, and create the compressed image.

In this project, the first and third steps, reading from and writing into images, are considered as bonus.
You are to focus on the second part of the process: clustering.

From a file listing all the pixels of the image with their position and color, regroup them into a given number of clusters, according to their colors.

See the subject for further details !

========================

Grade : A | Mark : 17.3

| Category       | Percentage | Tests     | Crash ? |
|----------------|------------|-----------|---------|
| 1 cluster      | 100%       | 4/4       | x       |
| 2 clusters     | 95.5%      | 21/22     | x       |
| 3 clusters     | 100%       | 7/7       | x       |
| 4 clusters     | 100%       | 6/6       | x       |
| 5 clusters     | 100%       | 4/4       | x       |
| 16 clusters    | 0%         | 0/1       | x       |
| Error handling | 100%       | 4/4       | x       |
| Coding Style   | 75%        | 3/4       | x       |
| **End score**  | **94.2%**  | **49/52** | **No**  |
