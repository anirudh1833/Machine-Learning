// K MEANS PROJECT ////

// Your task will be to try to cluster clients of a Wholesale Distributor
// based off of the sales of some product categories

// Source of the Data
//http://archive.ics.uci.edu/ml/datasets/Wholesale+customers

// Here is the info on the data:
// 1)	FRESH: annual spending (m.u.) on fresh products (Continuous);
// 2)	MILK: annual spending (m.u.) on milk products (Continuous);
// 3)	GROCERY: annual spending (m.u.)on grocery products (Continuous);
// 4)	FROZEN: annual spending (m.u.)on frozen products (Continuous)
// 5)	DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous)
// 6)	DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous);
// 7)	CHANNEL: customers Channel - Horeca (Hotel/Restaurant/Cafe) or Retail channel (Nominal)
// 8)	REGION: customers Region- Lisnon, Oporto or Other (Nominal)

// Import SparkSession
import org.apache.spark.sql.SparkSession

// Import log4j to set the Error reporting
import org.apache.log4j._
Logger.getLogger("org").setLevel(Level.ERROR)

// Create a Spark Session Instance
val spark = SparkSession.builder().getOrCreate()

// Import Kmeans clustering Algorithm
import org.apache.spark.ml.clustering.KMeans

// Load the Wholesale Customers Data
val dataset = spark.read.option("header","true").option("inferSchema","true").csv("Wholesale customers data.csv")

// Select the columns for the training set:
val feature_data = dataset.select($"Fresh", $"Milk", $"Grocery", $"Frozen", $"Detergents_Paper", $"Delicassen")

// Import VectorAssembler and Vectors
import org.apache.spark.ml.feature.{VectorAssembler,StringIndexer,VectorIndexer,OneHotEncoder}
import org.apache.spark.ml.linalg.Vectors

// Create a new VectorAssembler object for the feature
val assembler = new VectorAssembler().setInputCols(Array("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen")).setOutputCol("features")

// Use the assembler object to transform the feature_data
val training_data = assembler.transform(feature_data).select("features")

// Create a Kmeans Model with K=3
val kmeans = new KMeans().setK(3).setSeed(1L)

// Fit that model to the training_data
val model = kmeans.fit(training_data)

// Make predictions
val predictions = model.transform(training_data)

// Evaluate clustering by computing Silhouette score
val evaluator = new ClusteringEvaluator()
val silhouette = evaluator.evaluate(predictions)
println(s"Silhouette with squared euclidean distance = $silhouette")

// Shows the result.
println("Cluster Centers: ")
model.clusterCenters.foreach(println)
