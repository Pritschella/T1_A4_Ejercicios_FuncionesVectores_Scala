import scala.io.StdIn._
import scala.util.Sorting

object DatosEstadisticos {
  def llenarVector(n:Int): Array[Double]={
    var numeros=new Array[Double](n)
    numeros(0)=0.0
    for(i <- 0 until numeros.length){
      println("Ingrese un numero para el espacio "+ i + ":")
       numeros(i)= readDouble()
    }
    numeros
  }
  
  def imprimirVector(vector:Array[Double]):Unit={
    for(i <- 0 until vector.length){
      print(" "+vector(i)+" ")
    }
    println()
  }
  
  def ordenar(vector:Array[Double], n:Int):Array[Double]={
    var t=0.0
     for(i <- 0 to n-1){
         for(x <-0 to n-1){
             if(vector(i) < vector(x)){
               t=vector(i)
               vector(i)=vector(x)
               vector(x)=t
             }
         }
     }
     vector
  }
  def calcularMedia(vector:Array[Double]): Double ={
    var media=0.0
    var sumatoria=0.0
    for(i <- 0 until vector.length){
      sumatoria=sumatoria+vector(i)
    }
    media=sumatoria/vector.length
    media
  }
  def calcularModa(vector:Array[Double]): Double ={
    var max=0.0
    var moda=0.0
    for(i <- 0 until vector.length){
      var repeticion=0.0
      for(j <- 0 until vector.length){
        if(vector(i)==vector(j))
          repeticion+=1
      if(repeticion>max){
        moda=vector(i)
        max=repeticion
    }
  }
    }
    moda
  }
  
  def calcularMediana(vector:Array[Double]): Double ={  
    var mediana=0.0
    if(vector.length%2==0)
      mediana = ((vector((vector.length-1)/2)) + (vector(((vector.length-1)/2)+1)))/2
    else{
      mediana = vector(((vector.length)/2).toInt)
    }
    mediana
  }
  
  def desviacionRespectoDeLaMedia(vector:Array[Double]): Double ={ 
    var media=calcularMedia(vector)
    var moda=calcularModa(vector)
    var drm=moda-media
    drm
  }
  
  def desviacionMedia(vector:Array[Double]): Double ={ 
    var media=calcularMedia(vector)
    var sumatoria=0.0
   for(i <- 0 until vector.length)
      sumatoria=sumatoria+(vector(i)-media)
    
    var dm=sumatoria/vector.length
    dm
  }
  
  def calcularVarianza(vector:Array[Double]): Double ={ 
   var media=calcularMedia(vector)
    var sumatoria=0.0
   for(i <- 0 until vector.length)
      sumatoria=sumatoria+( (vector(i)-media)* (vector(i)-media) )
    
    var varianza=sumatoria/vector.length
    varianza
  }
  
  def calcularDesviacionEstandar(vector:Array[Double]): Double ={ 
    var varianza=calcularVarianza(vector)
    val desviacionEstandar=math.sqrt(varianza)
    desviacionEstandar
  }

  def main(args: Array[String]): Unit = {
    println("Ingrese el tamaño del vector: ")
    var n=readInt()
    var vector=llenarVector(n)  
    
    var menu=10
    while(menu>0){
      //Impresion de menu
      println("============== MENÚ ===========")
      println("Selecciona una opcion...")
      println("1) Imprimir vector")
      println("2) Ordenar vector")
      println("3) Calcular media")
      println("4) Calcular moda")
      println("5) Calcular mediana")
      println("6) Desviación respecto de la media")
      println("7) Desviación media")
      println("8) Varianza")
      println("9) Desviación estándar")
      println("0) Salir")
      menu=readInt()
      
      //Opcion impresion
      if(menu==1){
        imprimirVector(vector)
      }
     
     //Opcion ordenar
     if(menu==2){
       vector= ordenar(vector,n)
       imprimirVector(vector)
     }
     
     //Opcion calcular media
     if(menu==3){
       val media=calcularMedia(vector)
       println("La media es: "+media)
     }
     
     //Caclular moda
     if(menu ==4){
       val moda=calcularModa(vector)
       println("La moda es: "+moda)
     }
     
     //Caclular mediana
     if(menu ==5){
       vector= ordenar(vector,n)
       val mediana=calcularMediana(vector)
       println("La mediana es: "+mediana)
     }
     
      //Caclular la desviacion respecto de la media
     if(menu ==6){
       val drm=desviacionRespectoDeLaMedia(vector)
       println("La desviacion respecto de la media es: "+drm)
     }
     
     //Caclular la desviacion media
     if(menu ==7){
       val dm=desviacionMedia(vector)
       println("La desviacion media es: "+dm)
     }
     
     //Caclular la varianza
     if(menu ==8){
       val varianza=calcularVarianza(vector)
       println("La varianza es: "+varianza)
     }
     
      //Caclular la desviacion estandar
     if(menu ==9){
       val desviacion=calcularDesviacionEstandar(vector)
       println("La desviacion estandar es: "+desviacion)
     }
     
    }
    
  }
}