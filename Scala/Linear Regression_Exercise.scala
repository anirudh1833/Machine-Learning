//// LINEAR REGRESSION EXERCISE ///////////

// Import LinearRegression
import org.apache.spark.ml.regression.LinearRegression

// Import log4j to set the Error reporting
import org.apache.log4j._
Logger.getLogger("org").setLevel(Level.ERROR)

// Start a simple Spark Session
import org.apache.spark.sql.SparkSession
val spark = SparkSession.builder().getOrCreate()

// Use Spark to read in the Ecommerce Customers csv file.
val data = spark.read.option("header","true").option("inferSchema","true").csv("Clean_Ecommerce.csv")

// Print the Schema of the DataFrame
data.printSchema()

// Print out an example Row
val colnames = data.columns
val firstrow = data.head(1)(0)
println("\n")
println("Example Data Row")
for(ind <- Range(1,colnames.length)){
  println(colnames(ind))
  println(firstrow(ind))
  println("\n")
}

//// Setting Up DataFrame for Machine Learning ////
// Import VectorAssembler and Vectors
import org.apache.spark.ml.feature.VectorAssembler
import org.apache.spark.ml.linalg.Vectors

// Name the target column as "label"
val df = data.select(data("Yearly Amount Spent").as("label"),$"Avg Session Length",$"Time on App",$"Time on Website",$"Length of Membership")

// Use VectorAssembler to convert the input columns of df
val assembler = new VectorAssembler().setInputCols(Array("Avg Session Length","Time on App","Time on Website","Length of Membership")).setOutputCol("features")

// Use the assembler to transform our DataFrame to the two columns: label and features
val output = assembler.transform(df).select($"label",$"features")

// Create a Linear Regression Model object
val lr = new LinearRegression()

// Fit the model to the data and call this model lrModel
val lrModel = lr.fit(output)

// Print the coefficients and intercept for linear regression
println(s"Coefficients: ${lrModel.coefficients} Intercept: ${lrModel.intercept}")

// Summarize the model over the training set and print out some metrics!
val trainingSummary = lrModel.summary

// Show the residuals, the RMSE, the MSE, and the R^2 Values.
trainingSummary.residuals.show()
println(s"RMSE: ${trainingSummary.rootMeanSquaredError}")
println(s"MSE: ${trainingSummary.meanSquaredError}")
println(s"r2: ${trainingSummary.r2}")
