package directed_graph;

public class Rotate {

    public String[][] rotateMatrixBy90DegreeClockwise(String[][] matrix) {

        int totalRowsOfRotatedMatrix = matrix[0].length; //Total columns of Original Matrix
        int totalColsOfRotatedMatrix = matrix.length; //Total rows of Original Matrix

        String[][] rotatedMatrix = new String[totalRowsOfRotatedMatrix][totalColsOfRotatedMatrix];

        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[0].length; j++) {
                rotatedMatrix[j][(totalColsOfRotatedMatrix - 1) - i] = matrix[i][j];
            }
        }
        return rotatedMatrix;
    }

    //Rotate Matrix to 90 degree toward Left(counter clockwise)
    public String[][] rotateMatrixBy90DegreeCounterClockwise(String[][] matrix) {

        int totalRowsOfRotatedMatrix = matrix[0].length; //Total columns of Original Matrix
        int totalColsOfRotatedMatrix = matrix.length; //Total rows of Original Matrix

        String[][] rotatedMatrix = new String[totalRowsOfRotatedMatrix][totalColsOfRotatedMatrix];

        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[0].length; j++) {
                rotatedMatrix[(totalRowsOfRotatedMatrix - 1) - j][i] = matrix[i][j];
            }
        }
        return rotatedMatrix;
    }

}
