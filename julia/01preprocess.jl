using Pkg
Pkg.activate(".")
#Pkg.add("CSV")
#Pkg.add("DataFrames")
#Pkg.add("Plots")
#Pkg.add("PyPlot")
#Pkg.add("PlotlyJS")
using CSV
using DataFrames
using Dates
using Plots

datFiles = filter(x -> endswith(x, "csv"),  readdir("data"))
dat = CSV.read("data/" * datFiles[1], DataFrame, delim=",", header=["dati", "tempC", "hum"], skipto=2, )
first(dat, 6)
size(dat)

first(dat, 6)

for i in datFiles[2:end]
    global dat
    a = CSV.read("data/" * i, DataFrame, delim=",", header=["dati", "tempC", "hum"], skipto=2, )
    append!(dat, a)
end
size(dat)   
dat[!,:dati] = DateTime.(dat[!,:dati], "yyyy-mm-dd HH:MM:SS")
unique(dat) |> size
first(dat, 6)
scatter(dat[!,:dati], dat[!,:tempC], label="Temp (C)")
scatter!(dat[!,:dati], dat[!,:hum], label="Hum (%)")

# plot hum and temp
scatter(dat[!,:tempC], dat[!,:hum],label="")
xlabel!("Temp (C)")
ylabel!("Hum (%)")

#plotlyjs()
scatter(dat[!,:tempC], dat[!,:hum], dat[!, :dati],label="")
xlabel!("Temp (C)")
ylabel!("Hum (%)")
