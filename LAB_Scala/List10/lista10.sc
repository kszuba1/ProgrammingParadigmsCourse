def finMinMatrix(matrix:Array[Array[Int]])={
  def helper(i: Int,minArr:Array[Int]):Array[Int]={
    if i==matrix.length then minArr
    else {
      val tempArr = new Array[Int](1)
      tempArr(0) = (matrix(i) foldLeft (matrix(i)(0))) ((min, h) => if min==h then h else if min>h then h else min)
      helper(i+1,minArr.concat(tempArr))
    }
  }
  helper(0,new Array[Int](0))
}
val arr1 = new Array[Int](3)
arr1(0)=2
arr1(1)=1
arr1(2)=4
arr1
var arr2 = new Array[Int](3)
arr2(0)=(-2)
arr2(1)=(-3)
arr2(2)=(-4)
arr2

var arr3 = new Array[Array[Int]](2)
arr3(0)=arr1
arr3(1)=arr2
arr3
finMinMatrix(arr3)

def minMatrixImp(matrix:Array[Array[Int]])={
  val minArr = new Array[Int](matrix.length)
  for(i <- 0 to matrix.length-1) {
    minArr(i)=matrix(i)(0)
    for(j <- 0 to matrix(i).length-1){
      if matrix(i)(j)<minArr(i) then minArr(i)=matrix(i)(j)
    }
  }
  minArr
}
arr3
minMatrixImp(arr3)





