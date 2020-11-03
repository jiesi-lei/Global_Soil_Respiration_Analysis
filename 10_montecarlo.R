# This script is modified from Bond-Lamberty et.al (2010).
# Our sincerest thanks goes to Dr. Bond-Lamberty for providing open source database and data processing insights

# Program: 1-summarize_climate_and_soil_properties.r
-----------------------------------------------------------------------------
processFile <- function( fn_prefix, year, fields, func, firstfile )
{
# Read in 'filename'; apply sum_func to the monthly data; return data frame
	filename <- paste( fn_prefix, year, sep="" )
	print(paste( "Processing", filename ))
	a <- read.csv( file=filename, col.names=fields, header=FALSE, sep="" , row.names = NULL)
	
	# Apply passed-in function to data set (normally mean for temp, sum for precip)
	# There's probably a slick, one-line way to do this...
	lngth <- length( a[,1] )
for (i in 1:lngth)
	{
		a[i,"avg"] <- round( func( c(a[i,"jan"], a[i,"feb"], a[i,"mar"], a[i,"apr"], a[i,"may"], a[i,"jun"],
						a[i,"jul"], a[i,"aug"], a[i,"sep"], a[i,"oct"], a[i,"nov"], a[i,"dec"] ) ), digits=2 )
	}
	
	# Ready to write out the summary data	
	a$year <- year
	b<-data.frame( a["year"],a["long"], a["lat"], a["avg"] )
	if( firstfile )
	{
		write.csv( b, file=paste( fn_prefix, "avg", sep=""), row.names=FALSE )
	} else
	{
		write.table( b, file=paste( fn_prefix, "avg", sep=""), col.names =FALSE,  
			append=TRUE, row.names=FALSE, sep = "," )
	}
}

# ------------------------------------------
# ----------     MAIN PROGRAM     ---------- 
# ------------------------------------------

print( paste( date(), "Start 1-summarize_climate.r" ) )

fields=c("long","lat","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","avg")

for (i in 1948:2016)
{
	processFile( "Terrestial Precipitation and Temperature/air_temp_2017/air_temp.", i, fields, mean, i==1948 )
}

for (i in 1948:2016)
{
	processFile( "Terrestial Precipitation and Temperature/precip_2017/precip.", i, fields, sum, i==1948 )
}

print( paste( date(), "All done." ) )

#---------------------------------------------------------------------------------------------------------------------
#Extract values from raster to derive global SOC stock and elevations
library("raster")
library("rgdal")
library(maps)
library(ggplot2)

SOC <- raster("OCSTHA_M_30cm_250m_ll.tif")
plot(SOC, main="Global Soil Organic Carbon", xlab="Longitude",ylab="Latitude",cex.axis=1.3, cex.lab=1.4, cex.main=1.5,col=rev(heat.colors(10)))
coorddata <- read.csv("Terrestial Precipitation and Temperature/air_temp_2017/air_temp.avg.avg", header=TRUE, sep = "," , stringsAsFactors = FALSE)
coords <- data.frame(coorddata[,1:2])
coordinates(coords)<-c("long","lat")
#install.packages("maps")

map()
points(coords, pch=16)
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
#SOC <- spTransform(SOC,crs(coords))
compareCRS(SOC,coords)

soc.val<-extract(x=SOC, y=coords, method = 'simple',df = TRUE)
coorddata$SOC = soc.val$OCSTHA_M_30cm_250m_ll
coorddata = coorddata[,-3]
write.table( coorddata, file="SOC.gridcells", sep = ",", append=FALSE, row.names=FALSE )



ggplot() + 
  geom_histogram(data = val, aes(x = OCSTHA_M_30cm_250m_ll)) +
  ggtitle("Histogram of SOC (t/ha)") +
  xlab("SOC") + 
  ylab("Frequency of Pixels")

#library(elevatr)
#prj_dd <- "+proj=longlat +datum=WGS84 +no_defs"
#df_elev_epqs <- get_elev_point(coords, prj = prj_dd, src = "epqs")
DEM <- raster("DEM.tif")
plot(DEM, main="Digital Elevation Model", xlab="Longitude",ylab="Latitude",cex.axis=1.3, cex.lab=1.4, cex.main=1.5,col=rev(heat.colors(10)))
elev.val<-extract(x=DEM, y=coords, method = 'simple',df = TRUE)
coorddata$Elevation = elev.val$DEM
coorddata = coorddata[,-3]
write.table( coorddata, file="Elev.gridcells", sep = ",", append=FALSE, row.names=FALSE )


# Program: 2-compute_mat.r
# This program uses the output from
# program #1, which summarizes the monthly data into annual numbers.

-----------------------------------------------------------------------------
processFile <- function( filename, firstyear, lastyear, fields )
{
# Read in 'filename'; compute mean for all years within bounds for each grid cell
	a <- read.csv( file=filename, col.names=fields, header=TRUE, sep="," ,stringsAsFactors = FALSE)
	a$year = as.numeric((a$year))
	a$long = as.numeric((a$long))
	a$lat = as.numeric((a$lat))
	a$avg = as.numeric((a$avg))
	print("Checking year eligibility...")
	# Which rows are within bounds?
	x <- (a$year >= firstyear) & (a$year <= lastyear)
	a1 <- a[x,]

	print("Aggregating...")	
	b <- aggregate( a1, by=list(a1$long, a1$lat), FUN=mean)
	
	# Ready to write out the summary data

	print("Writing...")	
	c <- data.frame( b["long"], b["lat"], b["avg"] )
	c$avg <- round( c$avg, 2 )
	write.table( c, file=paste( filename, ".avg", sep="" ), sep = ",", append=FALSE, row.names=FALSE )
	print("File summary done")
}


# ------------------------------------------
# ----------     MAIN PROGRAM     ---------- 
# ------------------------------------------

print( paste( date(), "Start 2-compute_mat.r" ) )

fields=c( "year", "long", "lat", "avg" )

processFile( "Terrestial Precipitation and Temperature/air_temp_2017/air_temp.avg", 1987, 2016, fields )

processFile( "Terrestial Precipitation and Temperature/precip_2017/precip.avg", 1987, 2016, fields )

print( paste( date(), "All done." ) )

# Program: 3-lookup.r
# R Script to assign closest temperature points to transect location points.
# Uses the splancs R library (which loads sp as a foundation)
#
# August 7, 2009
# Ben Bond-Lamberty, JGCRI, PNNL
#
# AssignClosestPoints function based on a script by Rick Reeves from
#	http://www.nceas.ucsb.edu/scicomp/GISSeminar/UseCases/AssignClosestPointsR/AssignClosestPointsR.html

# -----------------------------------------------------------------------------
library("sp")
AssignClosestPoints <- function( long, lat, lookupdata, fieldname )
{
#	print(paste("AssignClosestPoints",long,lat,fieldname))
#	print(lookupdata)
  
	if ( is.na( long ) | is.na( lat ) )	# should filter these out ahead of time
	{
		return ( NA )
	}
	
	pttbl1 <- matrix( c( lookupdata$long, lookupdata$lat ), nrow=length( lookupdata$long ), ncol=2, byrow=FALSE )
	pttbl2 <- c( long, lat )

# Use the spDistsN1 function to compute the distance vector between the 
# data point and all of the temperature stations. Then, find and return
# the actual temperature (or whatever field is requested)
#
# We use Great Circle distance to increase distance calculation accuracy at high latitudes
 
	distVec <- spDistsN1( pttbl1, pttbl2, longlat = TRUE )

	closestSite <- which.min( distVec )
	d <- lookupdata[closestSite, fieldname]	
	print( paste( "Closest point value is #", closestSite, "value=", d ) )
	return( d )
}

# -----------------------------------------------------------------------------
LookUpTempPrecipData <- function( lookupfilename, fieldname, matchyear, updateonly=TRUE )
{
# For each set of coordinates in 'filename' with associated year(Y) and datayears(DY):
#	-if 'matchyear' is FALSE, just call assignclosestpoints
#	-otherwise, loop i from (Y - (DY-1)/2) to (Y + (DY-1)/2)
#		-filter lookupdata for entries where year=i
#		-look up closest point's data
#	-assign mean of those lookups to result vector

#	datapoints <- datapoints_raw[datapoints_raw$YearsOfData >= 1 &
#		!is.na( datapoints_raw$YearOfData),] 	# Filter out invalid recs

	print( paste( "Opening", lookupfilename, "..." ) )
	lookupdata <- read.csv( lookupfilename, header=TRUE, sep = "," , stringsAsFactors = FALSE)
	#lookupdata$year = as.numeric(lookupdata$year)
	lookupdata$long = as.numeric(lookupdata$long)
	lookupdata$lat = as.numeric(lookupdata$lat)
	lookupdata$avg = as.numeric(lookupdata$avg)
	summary(lookupdata)

	print( paste( "Processing", nrow( datapoints ), "records..." ) )
	for ( i in 1:nrow( datapoints ) )
	{
		print( paste( "Processing #", i, "@", round(datapoints[i, "Longitude"], 2), ",",
			round( datapoints[i, "Latitude2"], 2 ), "midyear=", datapoints[i, "Study_midyear"], 
			"years=", datapoints[i, "YearsOfData"] ) )

		if ( is.na( datapoints[i, "Study_midyear"] ) || is.na( datapoints[i, "YearsOfData"] ) ||
			is.na( datapoints[i, "Longitude"] ) || is.na( datapoints[i, "Latitude2"] || 
			(datapoints[i, "YearsOfData"] < 1 ) ) )
		{
			next
		}
		if ( updateonly && !is.null( datapoints[i, fieldname]) && !is.na( datapoints[i, fieldname] ) )
		{
			print("Skip!")
			next
		}
		if ( matchyear )
		{
			radius <- ( round( datapoints[i, "YearsOfData"], digits=0 ) -1 )/2
			count <- 0
			val <- 0
			for ( j in ( datapoints[i, "Study_midyear"]-radius ) : ( datapoints[i, "Study_midyear"]+radius ) )
			{
#				print(paste("Filter for year", trunc(j)))
				val <- val + AssignClosestPoints( datapoints[i, "Longitude"], datapoints[i, "Latitude2"], 
					lookupdata[lookupdata$year == trunc(j),], "avg")
				count <- count+1
				
				if (j != trunc(j))	# fractional year, so need to average nearest two
				{
#					print(paste("Filter for secondary year", trunc(j+0.5)))
					val <- val + AssignClosestPoints( datapoints[i,"Longitude"], datapoints[i,"Latitude2"], 
						lookupdata[lookupdata$year == trunc(j+0.5),], "avg")
					count <- count+1
				}
			}
		}
		else # not required to match a specific year
		{
#			print("No filter")
			val <- AssignClosestPoints( datapoints[i,"Longitude"], datapoints[i,"Latitude2"], 
				lookupdata, "avg")
			count <- 1
		}
		print(paste("...assign mean", val/count, " mean of", count, "values to", fieldname))
		datapoints[i, fieldname] <- val / count		# Note super-assignment
	}
	
	print( paste( "Finished processing ", lookupfilename ) )
	print( "-----------------------------------------------------------------------------")
}

# ------------------------------------------
# ----------     MAIN PROGRAM     ---------- 
# ------------------------------------------

library( sp )  

print( paste( date(), "Start 3-lookup.r" ) )

datapoints <- read.csv( "03_processed_data_complete_final.csv" )		# this should have a header line and be CSV

#debug( LookUpTempPrecipData )

LookUpTempPrecipData( "Terrestial Precipitation and Temperature/air_temp_2017/air_temp.avg", "LU_AT", TRUE )
LookUpTempPrecipData( "Terrestial Precipitation and Temperature/precip_2017/precip.avg", "LU_AP", TRUE )
LookUpTempPrecipData( "Terrestial Precipitation and Temperature/air_temp_2017/air_temp.avg.avg", "LU_MAT", FALSE )
LookUpTempPrecipData( "Terrestial Precipitation and Temperature/precip_2017/precip.avg.avg", "LU_MAP", FALSE )
#LookUpTempPrecipData( "lai_data/lai_10.csv", "LU_LAI", FALSE )
#LookUpTempPrecipData( "ndep_data/ndep5deg.csv", "LU_NDEP", FALSE )

outfile <- "srdb_out.csv"
print( paste( "Processing is complete; saving data to", outfile ) )
write.csv( datapoints, file=outfile, row.names=FALSE )

print( paste( date(), "All done." ) )


# Program: 4-analyze.r
# R Script to analyze the soil respiration database
# Specifically looking for temporal trends in annual Rs flux, and what explains them

# August 2009
# Ben Bond-Lamberty, JGCRI, PNNL

transcript <- FALSE			# record transcripts to file (and don't wait for user)?

# Note model parameters in 'regress' function below have to be adjusted when changing model_set!
model_set <- "NOLAI-"


# -----------------------------------------------------------------------------
print_trend <- function( m )
{
	trend_temp <- ( ( m$coefficients["LU_MAT"] )^2 ) * 100
	trend_precip <- ( ( m$coefficients["LU_MAP"] )^2 ) * 100
	trend_Tanomaly <- ( ( m$coefficients["T_anomaly"] )^2 ) * 100
	trend_Panomaly <- ( ( m$coefficients["P_anomaly"] )^2 ) * 100
	trend_unknown <- (( m$coefficients["Study_midyear"] )^2 ) * 100
	if( !is.na( trend_temp ) )
	{
		print( paste( "Temperature trend is", round( trend_temp, 1 ), "% per degC" ) )
	}
	if( !is.na( trend_precip ) )
	{
		print( paste( "Precipitation trend is", round( trend_precip, 1 ), "% per mm" ) )
	}
	if( !is.na( trend_Tanomaly ) )
	{
		print( paste( "Temp anomaly trend is", round( trend_Tanomaly, 1 ), "% per degC" ) )
	}
	if( !is.na( trend_Panomaly ) )
	{
		print( paste( "Precip anomaly trend is", round( trend_Panomaly, 1 ), "% per degC" ) )
	}
	if( !is.na( trend_unknown ) )
	{
		print( paste( "Unknown time trend is", round( trend_unknown, 1 ), "% per year" ) )
	}
}

# -----------------------------------------------------------------------------
check_linearity <- function( srdb )
{
	# R&S model - MAT and MAP
	m_m0 <- lm( sqrt( Rs_annual ) ~ LU_MAT + LU_MAP + I(LU_MAT^2) + I(LU_MAP^2),
									weights=YearsOfData,
									data=srdb )
	par( mfrow=c( 2,2 ) )
	plot( m_m0, labels.id=paste( srdb$Study, srdb$Author ), main="Linearity check!" )
	readline()
	
	r <- residuals( m_m0 )
	par( mfrow=c( 1,1 ) )
	plot( srdb$Study_midyear, r[srdb$Num], main="Trend in residuals from R&S-type model?",
		xlab="Year of study", ylab="Residual from MAT+MAP model" )
	rmodel <- lm( r[srdb$Num] ~ srdb$Study_midyear )
	abline( rmodel$coefficients, col="red", lty=2 )
	readline()
}

# -----------------------------------------------------------------------------
regress <- function( srdb, dataname, depvar="Rs_annual" ) 
{
	# Filter out some bad 'uns
	srdb <- srdb[srdb[, depvar] > 0,]							# only positive numbers

#	srdb <- srdb[!is.na( srdb$LAI ),]

	print( "----------------------------------------------------------------------------")
	print( paste( "Now running analysis", dataname, "; records =", nrow( srdb), 
		"; depvar =", depvar ) )
	print( "----------------------------------------------------------------------------")

	if( nrow( srdb ) < 50 )
	{
		print( "*** ABORTING *** too few data for this analysis" )
		return()
	}

#	print( summary( lm( sqrt( srdb[, depvar] ) ~ 1 + LU_MAT + LU_MAP +
#		Study_midyear, weights=YearsOfData, data=srdb ) ) )

	print( "----- Base model -----" )
	m_base <- lm( sqrt(srdb[, depvar]) ~ 1 +	
	                LU_AT + I( LU_AT^2 ) +
	                LU_AP + I( LU_AP^2 ) +
	                LU_AT*LU_AP +
	                LU_AT*T_anomaly +	
	                LU_AP*P_anomaly +
	                Study_midyear, 
	              weights=YearsOfData,
	              data=srdb )
	
	# Minimal model - MAT and MAP and Study_midyear
	m_min <- lm(sqrt(srdb[, depvar])  ~ 1 + LU_AT + LU_AP,  weights=YearsOfData, data=srdb )
	
	# Maximal model - everything
	m_max <- m_base <- lm( sqrt(srdb[, depvar]) ~ 1 +	
	                         LU_AT + I( LU_AT^2 ) +
	                         LU_AP + I( LU_AP^2 ) +
	                         LU_AT*LU_AP +
	                         LU_AT*T_anomaly +
	                         LU_AP*P_anomaly, 
	                       weights=YearsOfData,
	                       data=srdb )
	
	
	# Use step to produce a reduced model
  m_red <- step( m_max, scope = c( upper=m_max, lower=m_min ) )
	#m_red  = m_max
	print( summary( m_red ) )
	print( paste( "AIC =", round( AIC( m_red ), 0 ) ) )
	print( paste( "N =", length( m_red$residuals ) ) )

	if( !transcript )
	{
		par( mfrow=c( 2,2 ) )
		plot( m_red, labels.id=paste( srdb$Study, srdb$Author ), 
				main=paste( dataname, "reduced model" ) )
	}
	save( m_red, file=paste( "models/", model_set, dataname, ".Rdata", sep="" ) )#manually create a folder named "models" beforehand
	
	# Check for influential outliers
	influentials <- which( cooks.distance( m_red ) > 0.5 )
	cooks_count <- length( influentials )
	print( paste( "Influential outliers =", cooks_count ) )
	if ( cooks_count > 0 )
	{
		print( "Press RETURN to re-run analysis without those influential outliers..." )
		if( !transcript )
		{
			readline()	
		}
		# Delete observations from data set and recursively call regress
		regress( srdb[-influentials,], paste( dataname, "-NO INFLUENTIALS", sep="" ) )
	} else
	{
		print( paste( "Press RETURN to finish analysis", model_set, dataname, "..." ) )
		if( !transcript )
		{
			readline()	
		}
	}
}


# ------------------------------------------
# ----------     MAIN PROGRAM     ---------- 
# ------------------------------------------

# library( MASS )			# Needed for robust regression (rlm)

print( paste( date(), "Start 4-analyze.r" ) )

if( transcript )
{
	sink( paste( "models0731/", model_set, "transcript.txt", sep="" ) )
}

infile="srdb_out.csv"
srdb <- read.csv( infile, header=TRUE )
print( paste( infile, "has", nrow( srdb ), "records" ) )

# Do some filtering to produce the 'base' data set
#srdb <- srdb[srdb$Manipulation == "None",]					# only non-manipulated systems
srdb <- srdb[srdb$Ecosystem_type != "Agriculture",]			# no agriculture
srdb <- srdb[srdb$Rs_annual > 0,]
srdb <- srdb[!is.na( srdb$LU_MAT ),]

# Compute the anomalies--it makes results easier to understand
srdb$T_anomaly <- srdb$LU_AT - srdb$LU_MAT
srdb$P_anomaly <- srdb$LU_AP - srdb$LU_MAP


write.csv( srdb, "srdb_out2.csv", row.names=FALSE )

# This line was a separate analysis at bottom, but it's pretty critical to have up here.
# Most early measurements were soda lime and alkali absorption techniques, which were
# notorious for under-measuring CO2 fluxes.
srdb_all <- srdb
#srdb <- srdb[!srdb$CO2_method %in% c( "Soda lime", "Alkali absorption" ),]
#srdb <- srdb[srdb$Study_midyear > 1988,]

# Is the basic model form appropriate?
#check_linearity( srdb )

# Base analysis: Annual Rs, global, all non-ag ecosystems w/ modern measurement techniques
regress( srdb, "BASE_ANALYSIS" )

if( transcript )
{
	sink()
}

print( paste( date(), "All done." ) )

# Program: 5-globalcells.r
# R Script to match global cell areas with climate and LAI data for those cells

# September 2009
# Ben Bond-Lamberty, JGCRI, PNNL

# AssignClosestPoints function based on a script by Rick Reeves from
#	http://www.nceas.ucsb.edu/scicomp/GISSeminar/UseCases/AssignClosestPointsR/AssignClosestPointsR.html

# -----------------------------------------------------------------------------
AssignClosestPoints <- function( long, lat, lookupdata, fieldname="avg" )
{
	
	pttbl1 <- matrix( c( lookupdata$long,lookupdata$lat ), nrow=length( lookupdata$long ), ncol=2, byrow=FALSE )
	pttbl2 <- c( long, lat )

	# Use the spDistsN1 function to compute the distance vector between the 
	# data point and all of the temperature stations. Then, find and return
	# the actual temperature (or whatever field is requested)
	#
	# We use Great Circle distance to increase distance calculation accuracy at high latitudes
 
	distVec <- spDistsN1( pttbl1, pttbl2, longlat = TRUE )

	closestSite <- which.min( distVec )
	d <- lookupdata[closestSite, fieldname]	
	#	print( paste( "Closest point value is #", closestSite, "value=", d ) )
	return( d )
}

# -----------------------------------------------------------------------------
recompute_cells <- function( cellsfile, oldcellsfile="", 
	lu_at=TRUE, lu_ap=TRUE, lu_mat=TRUE, lu_map=TRUE,lu_soc = TRUE, lu_elev = TRUE, lu_lat = TRUE )
{
	do_year <- function( cells, year, cellsfile, writeheader ) # nested function
	{
		# Subset data
		if( lu_at ) lu_at_table <- lu_at_table[lu_at_table$year == year,]
		if( lu_ap ) lu_ap_table <- lu_ap_table[lu_ap_table$year == year,]
		
		print( paste( "Year", year, "; cells to process =", nrow( cells ), date() ) )
	
		lookup <- function( x )	# internal function that 'apply' will use, below
		{
			s1 <- lu_data[lu_data$long == x["long"] & lu_data$lat == x["lat"], ]
			if ( nrow( s1 )==1 )
			{
				return( s1[1, "avg"] )
			} else
			{
				# Probably we're at a really extreme latitude that the met data doesn't cover
				# so just pick the closest points (very slow computationally)
				return( AssignClosestPoints( x["long"], x["lat"], lu_data, "avg" ) )
			}
		}

		if( lu_at )
		{
				print( "  Looking up lu_at_table..." )
				lu_data <- lu_at_table
				cells$LU_AT <- apply( cells, 1, lookup )
		}		
		if( lu_mat )
		{
			print( "  Looking up lu_mat_table..." )
			lu_data <- lu_mat_table
			cells$LU_MAT <- apply( cells, 1, lookup )
		}
		if( lu_ap )
		{
			print( "  Looking up lu_ap_table..." )
			lu_data <- lu_ap_table
			cells$LU_AP <- apply( cells, 1, lookup )
		}
		if( lu_map )
		{
			print( "  Looking up lu_map_table..." )
			lu_data <- lu_map_table
			cells$LU_MAP <- apply( cells, 1, lookup )
		}
		if( lu_soc )
		{
			print( "  Looking up lu_soc_table..." )
			lu_data <- lu_soc_table
			cells$LU_SOC <- apply( cells, 1, lookup )
		}
		if( lu_elev )
		{
			print( "  Looking up lu_elev_table..." )
			lu_data <- lu_elev_table
			cells$LU_Elev <- apply( cells, 1, lookup )
		}	
		if( lu_lat )
		{
		  print( "  Looking up lu_lat_table..." )
		  lu_data <- lu_lat_table
		  cells$LU_Lat <- apply( cells, 1, lookup )
		}	
		print( "  Done, appending to file")
		cells[, "year"] <- year
		print(summary(cells))
		write.table( cells, file=cellsfile, append=TRUE, row.names=FALSE, 
			col.names=writeheader, sep="," )
	}

	# Read in grid cell areas (terrestrial only)
	if( file.exists( oldcellsfile ) )
	{
		print( "Reading old cells file..." )
		cells <- read.csv( oldcellsfile )
	} else
	{
		cells <- read.csv( "cell_areas.txt", header=FALSE,
			col.names=c( "long", "lat", "area" ), skip=12 )
		cells <- cells[cells$area != -9999.0,]
	}

	# Read in temperatures, precip
	if( lu_at )
	{
		lookupfilename <- "Terrestial Precipitation and Temperature/air_temp_2017/air_temp.avg"
		print( paste( "Opening", lookupfilename, "..." ) )
		lu_at_table <- read.csv( lookupfilename )
	}	
	if( lu_mat )
	{
		lookupfilename <- "Terrestial Precipitation and Temperature/air_temp_2017/air_temp.avg.avg"
		print( paste( "Opening", lookupfilename, "..." ) )
		lu_mat_table <- read.csv( lookupfilename )
	}
	if( lu_ap )  
	{
		lookupfilename <- "Terrestial Precipitation and Temperature/precip_2017/precip.avg"
		print( paste( "Opening", lookupfilename, "..." ) )
		lu_ap_table <- read.csv( lookupfilename )
	}
	if( lu_map )
	{
		lookupfilename <- "Terrestial Precipitation and Temperature/precip_2017/precip.avg.avg"
		print( paste( "Opening", lookupfilename, "..." ) )
		lu_map_table <- read.csv( lookupfilename )
	}
	if( lu_soc )
	{
	  lookupfilename <- "SOC.gridcells"
	  print( paste( "Opening", lookupfilename, "..." ) )
	  lu_soc_table <- read.csv( lookupfilename )
	}
	if( lu_elev )
	{
	  lookupfilename <- "Elev.gridcells"
	  print( paste( "Opening", lookupfilename, "..." ) )
	  lu_elev_table <- read.csv( lookupfilename )
	}
	if( lu_lat )
	{
	  lookupfilename <- "Lat.gridcells"
	  print( paste( "Opening", lookupfilename, "..." ) )
	  lu_lat_table <- read.csv( lookupfilename )
	}		
	for ( i in 1987:2016 )
	{
  #if( file.exists( cellsfile ) )
  #{
	#		do_year( cells[cells$year == i,], i, cellsfile,(i==1987))
 #  } else
  #	{
   	do_year( cells, i, cellsfile,(i==1987))		# Very slow
 #  }
	}
}


# ------------------------------------------
# ----------     MAIN PROGRAM     ---------- 
# ------------------------------------------

library( sp )

print( paste( date(), "Start 5-globalcells.r" ) )

cellsfile <- "cells.csv"

if( file.exists( cellsfile ) )
{
	print( paste( "File", cellsfile, "exists. Use existing file (y/n)?" ) )
	yn <- readline()
	if( yn %in% c( "n", "N" ) )
	{
		stop( "Remove the existing file and we'll recompute." )
	}
} else
{
	print( "Computing cells lookup data..." )
	recompute_cells( cellsfile )

	# Note that recompute_cells has options to just recompute certain variables, updating
	# a previously-saved cells file (not used in the above call, though).
	# For instance here is call to update cells.csv with new ndep data:
	# recompute_cells("cells.csv", oldcellsfile="oldcells.csv", lu_at=FALSE, lu_ap=FALSE, 
	#	lu_mat=FALSE, lu_map=FALSE, lu_lai=FALSE, lu_ndep=TRUE)
}

print( paste( date(), "All done." ) )

# Program: 6-predict.r
# R Script to use a saved model to predict global soil respiration

# September 2009
# Ben Bond-Lamberty, JGCRI, PNNL


# -----------------------------------------------------------------------------
montecarlo <- function( mc_red, mc_runs, do_mc=TRUE )
{
	coefs <- summary( m_red )$coefficients

	# TEMPORARY: try setting boreal T_anomaly coefficient to 0 or temperate value
#	coefs["T_anomaly:genbiomeBoreal", 1] <- 0
#	coefs["T_anomaly:genbiomeBoreal", 1] <- -coefs["T_anomaly", 1]
#	coefs["T_anomaly:genbiomeBoreal", 2] <- 0 
	
	print( "Model coefficients are:" )
	print( coefs )
	
	m_red_mc <- m_red											# Monte Carlo model
	globalflux <- data.frame( matrix( NA, 
				ncol=length( unique( cells$Study_midyear ) ), 
				nrow=mc_runs ) )								# this holds MC results
	globalrhflux <- data.frame( matrix( NA, 
				ncol=length( unique( cells$Study_midyear ) ), 
				nrow=mc_runs ) )								# this holds MC results
	names( globalflux ) <- unique( cells$Study_midyear )
	
	print( paste( "Now starting", mc_runs, "Monte Carlo simulations..." ) )
	for( i in 1:mc_runs )	# the Monte Carlo loop
	{
		# Generate new parameter values from the original mean and s.e.
		if( do_mc )
		{
			m_red_mc$coefficients <- rnorm( n=nrow( coefs ), mean=coefs[, 1], sd=coefs[, 2] )
		}
	
		# Calculate Rs flux, by year, for each grid cell, using this randomly-generated model
		print( paste( "Calculating grid cell fluxes for model", i, "..." ) )
		cells$flux <<- predict( m_red_mc, newdata=cells )^2
			# ^2 because the dependent variable (Rs_annual) was transformed	; no-sqrt transformation for Jiesi(2020)
		
		cells$rhflux <<- exp( 1.22 + 0.73 * log( cells$flux ) )
			# eqn from Bond-Lamberty et al. (2004b)
		

		cells$cellflux <<- cells$area * 1000 * 1000 * cells$flux / 1e15 	# Pg/yr
		cells$cellrhflux <<- cells$area * 1000 * 1000 * cells$rhflux / 1e15
		
		globalflux[i,] <- tapply( cells$cellflux, cells$Study_midyear, sum ) # save results
		globalrhflux[i,] <- tapply( cells$cellrhflux, cells$Study_midyear, sum ) # save results
		
		print( round( globalflux[i,], 1 ) )
	}
	return( list( rs=globalflux, rh=globalrhflux ) )
}


# ------------------------------------------
# ----------     MAIN PROGRAM     ---------- 
# ------------------------------------------

print( paste( date(), "Start 6-predict.r" ) )

set.seed( 2009 )		# we want reproducible runs

cellsfile <- "cells.csv"

# Load global cells w/ matched climate data
print( paste( "Reading", cellsfile, "..." ) )
#cells <- read.csv( "testcells.csv", header=TRUE )
cells <- read.csv( cellsfile, header=TRUE )

#cells <- cells[cells$year > 1988,]
print( paste( "Min year =", min( cells$year ) ) )
print( paste( "Max year =", max( cells$year ) ) )

n <- names( cells )
n[which( names( cells ) == "year" )] <- "Study_midyear"	# Rename this column to what model is expecting
n[which( names( cells ) == "LU_Elev" )] <- "Elevation"	
n[which( names( cells ) == "LU_SOC" )] <- "SOC"
n[which( names( cells ) == "LU_Lat" )] <- "Latitude"
names( cells ) <- n

cells$T_anomaly <- cells$LU_AT - cells$LU_MAT
cells$P_anomaly <- cells$LU_AP - cells$LU_MAP

cells$genbiome <- "Temperate"
cells[cells$LU_MAT > 16, "genbiome"] <- "Tropical"
cells[cells$LU_MAT < 2, "genbiome"] <- "Boreal"
cells$genbiome <- relevel( as.factor( cells$genbiome ), "Temperate" )
print( summary( cells$genbiome ) )
cells1 = filter(cells, is.na(SOC) == FALSE )
SOC_fill = tapply(cells1$SOC, cells1$genbiome, mean) #~10% of data lacked corresponding SOC, fill in SOC blanks with biome-median values
cells[(cells$genbiome == "Temperate") & (is.na(cells$SOC) == TRUE),"SOC"] <- SOC_fill[1]
cells[(cells$genbiome == "Boreal") & (is.na(cells$SOC) == TRUE),"SOC"] <- SOC_fill[2]
cells[(cells$genbiome == "Tropical") & (is.na(cells$SOC) == TRUE),"SOC"] <- SOC_fill[3]

library(dplyr)
cells = filter(cells, is.na(SOC) == FALSE & is.na(Elevation) == FALSE)# ~10% data lack SOC

# Load saved model
modelname <- "NOLAI-BASE_ANALYSIS"


savedmodel <- paste( "models/", modelname, ".Rdata", sep="" )
print( paste( "Reading model", savedmodel, "..." ) ) 
load( savedmodel )

# Do the Monte Carlo runs
mc_runs <- 1000
fluxes <- montecarlo( m_red, mc_runs )			# m_red has been loaded from 'savedmodel'
globalflux <- fluxes$rs

# Compute final stats
gf_final <- as.data.frame( round( apply( globalflux, 2 ,mean), 3 ) )
names( gf_final ) <- c( "rs_mean" )
gf_final$rs_sd <- round( apply( globalflux, 2 ,sd), 3 )
gf_final$year <- as( colnames( globalflux ), "numeric" )
gf_final$rh_mean <- ( round( apply( fluxes$rh, 2, mean ), 3 ) )
gf_final$rh_sd <- round( apply( globalflux, 2 ,sd), 3 )

# Compute mean global temperature (in this data set): cell temp weighted by area
gf_final$AT <- tapply( cells$area * cells$LU_AT, cells$Study_midyear, sum )
gf_final$AT <- round( gf_final$AT / tapply( cells$area, cells$Study_midyear, sum ), 2 )

# Compute the global Rs Q10 = (R2-R1)^(10/(T2-T1))
# To do this fit a simple linear model to temp and Rs and use endpoints for computation
m1 <- lm( rs_mean~year, data=gf_final )
m2 <- lm( AT~year, data=gf_final )
q10 <- ( predict( m1 )[nrow( gf_final )] / predict( m1 )[1] ) ^
	( 10 / ( predict( m2 )[nrow( gf_final )] - predict( m2 )[1] ) )
print( paste( "Q10 over", nrow( gf_final ), "years =", round( q10, 2 ) ) )

print( paste( "Mean flux: ", round( mean( gf_final$rs_mean ), 1 ) ) )
print( paste( "Flux change/yr: ", round( m1$coefficients[2], 2 ) ) )

# Save relevant data to files in 'results' folder
print( "Writing final result files..." )
year_mcruns <- paste( min(gf_final$year), "-", max(gf_final$year), "_", mc_runs, sep="" )
write.csv( gf_final, file=paste( "results/", modelname, "_gf_final_", year_mcruns, ".csv", sep="" ) )
write.csv( globalflux, file=paste( "results/", modelname, "_globalflux_", year_mcruns, ".csv", sep="" ) )
write.csv( cells, file=paste( "results/", modelname, "_cells_", year_mcruns, ".csv", sep="" ) )

print( paste( date(), "All done." ) )

#global flux----------------------------------------------------------------------------
library("dplyr")
library("reshape2")
library("knitr")
filename = "results/NOLAI-BASE_ANALYSIS_globalflux_1987-2016_1000.csv"
globalflux = read.csv(filename, header = TRUE)[,-1]
globalflux = as.data.frame(t(globalflux))
globalflux$year = c(1987:2016)
globalflux_transform<-melt(globalflux,id.vars=c("year"))

m0 <- lm( value~year, data=globalflux_transform)
m1 <- lm( value~year, data=filter(globalflux_transform,year <=1998 ))
m2 <- lm( value~year, data=filter(globalflux_transform,year <= 2014 & year >= 1999  ))
m3 <- lm( value~year, data=filter(globalflux_transform,year <=2014 ))
summary(m0)
summary(m1)
summary(m2)
summary(m3)

print( paste( "Mean flux: ", round( mean( gf_final$rs_mean ), 1 ) ) )
print( paste( "1987-1998 Mean flux: ", round( mean( filter(gf_final,year <=1998 )$rs_mean ), 1 ) ) )
print( paste( "1999-2014 Mean flux: ", round( mean( filter(gf_final,year <=2014 &year >=1999 )$rs_mean ), 1 ) ) )
print( paste( "1987-2014 Mean flux: ", round( mean( filter(gf_final,year <=2014 & year >=1987 )$rs_mean ), 1 ) ) )
print( paste( "1999-2016 Mean flux: ", round( mean( filter(gf_final,year <=2016 & year >=1999 )$rs_mean ), 1 ) ) )
print( paste( "Flux change/yr: ", round( m0$coefficients[2], 2 ) ) )
print( paste( "1987-1999 Flux change/yr: ", round( m1$coefficients[2], 2 ) ) )
print( paste( "2000-2016 Flux change/yr: ", round( m2$coefficients[2], 2 ) ) )
print( paste( "1998-2012 Flux change/yr: ", round( m3$coefficients[2], 2 ) ) )

m1997 <- lm( rs_mean~year, data=filter(gf_final,year <=1997 ))
round( m1997$coefficients[2], 2 )
summary(m1997)



# FIGURE-----------------------------------------------------------------------------
figure2 <- function( gf_final, trials )	# Draw a plot of global Rs flux over time
{
	def.par <- par( no.readonly = TRUE ) # save default, for resetting...

	gf_final$upper_sd <- gf_final$rs_mean + gf_final$rs_sd 
	gf_final$lower_sd <- gf_final$rs_mean - gf_final$rs_sd 

	plot( gf_final$year, gf_final$rs_mean, 
		xlab="Year", ylab=expression( R[S]~(Pg) ), las=1, 
		ylim=c( min( gf_final$lower_sd )-2, max( gf_final$upper_sd )+2 ), type="n" )

	# Light gray standard deviation area	
	xx <- c( gf_final$year, rev( gf_final$year ))
	yy <- c( gf_final$upper_sd, rev( gf_final$lower_sd ))
	polygon( xx, yy, col="lightgray", border=NA )

	# Gray 95% CI area	
	gf_final$upper_95ci <- gf_final$rs_mean + qnorm( 0.975 ) * gf_final$rs_sd / trials
	gf_final$lower_95ci <- gf_final$rs_mean - qnorm( 0.975 ) * gf_final$rs_sd / trials
	xx <- c( gf_final$year, rev( gf_final$year ))
	yy <- c( gf_final$upper_95ci, rev( gf_final$lower_95ci ))
	polygon( xx, yy, col="gray", border=NA )

	# Dark center line and regression lines
	fit1 <- lm(rs_mean ~ year, gf_final[gf_final$year <= 1998,] )
	summary(fit1)
	new1 <- data.frame(year = seq(1987, 1998))
	lines(new1$year, predict(fit1, new1))
	fit2 <- lm(rs_mean ~ year, gf_final[gf_final$year > 1998 & gf_final$year <= 2014,] )
	summary(fit2)
	new2 <- data.frame(year = seq(1999, 2014))
	lines(new2$year, predict(fit2, new2))
	#pre89 <- gf_final[gf_final$year <= 1989,]
	#post88 <- gf_final[gf_final$year > 1988,]	
	#lines( pre89$year, pre89$rs_mean, type="o", lwd=2, lty=2 )
	lines( gf_final$year, gf_final$rs_mean, type="o", lwd=4 )
	
	print( "2008 mean and sd:" )
	print( paste( gf_final[gf_final$year == 2008,"rs_mean"], "±", 
		gf_final[gf_final$year == 2008,"rs_sd"] ) )
	print( "Temporal trend?" )
	print( summary( lm( rs_mean ~ year, data=gf_final ) ) )
	print( paste( "Interannual variability:", round( sd( gf_final$rs_mean ), 1 ) ) )
	readline()

	par(def.par)
}

# ------------------------------------------
# ----------     MAIN PROGRAM     ---------- 
# ------------------------------------------

library( lattice )

print( paste( date(), "Start 8-figures.r" ) )


analysis <- "NOLAI-BASE_ANALYSIS"
period <- "1987-2016"
trials <- 1000
gf_final <- read.csv( paste( 
	"results/", analysis, "_gf_final_", period, "_", trials, ".csv", sep="" ) )


figure2( gf_final, trials )		# Draw a plot of global Rs flux over time




