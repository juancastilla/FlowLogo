import Jama.*;
import java.lang.Math.*;

public class sample{

public static void main(String[] args) {

  double[][] array = {{1.,2.,3},{4.,5.,6.},{7.,8.,10.}};
  Matrix A = new Matrix(array);
  Matrix b = Matrix.random(3,1);
  Matrix x = CholeskyDecomposition(A).solve(b);

  //Printing Answers
          System.out.println("x = " + Math.round(x.get(0, 0)));
          System.out.println("y = " + Math.round(x.get(1, 0)));
          System.out.println("z = " + Math.round(x.get(2, 0)));

  }



}
