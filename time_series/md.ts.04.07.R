rm(list = ls())
datasets = c("co2", "AirPassengers", "JohnsonJohnson", "uspop", "USPop", "lynx", "Nile", "sunspots")
 
dataplot = function(idx){
  par(mfrow = c(2,1))  # Set plotting area to 2 rows, 1 column
  dataset_name = datasets[idx]
  dataset = get(dataset_name)  # Get the actual dataset object
  plot(dataset, type= "l", main = dataset_name)  # Plot original dataset
  help(dataset_name)  # Show help in console
  plot(log(dataset), type= "l", main = paste("log", dataset_name))  # Plot log-transformed
}

dataplot(1)  # co2 dataset
dataplot(2)  # AirPassengers dataset
dataplot(3)  # JohnsonJohnson dataset
dataplot(4)  # uspop dataset
dataplot(5)  # USPop dataset
dataplot(6)  # lynx dataset
dataplot(7)  # Nile dataset
dataplot(8)  # sunspots dataset
