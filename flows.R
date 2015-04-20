# Script de R que calcula la creación y destrucción de empleo por meses o trimestres
# usando la MCVL.

rm(list=ls())

# Archivos guardados aquí:
path_to_files = "/path/to/files"

# Output guardado aquí:
path_to_output = "/path/to/output"

# Primero cargamos los datos de las personas
person <- read.table(paste0(path_to_file, "MCVL2012PERSONAL_CDF.TXT",
header=FALSE, sep=";", 
col.names=c("ident_pf", "a_naci","sexo","nac_clas","pro_nac","pro_naf","domic",
"f_fall", "pa_nac_cla","niv_est","borrar") ); 

# Borramos datos innecesarios para calcular los flujos
person$borrar <- NULL
person$pro_nac <- NULL; person$pro_naf <- NULL; person$domic<-NULL; 
person$pa_nac_cla<-NULL; person$niv_est<- NULL; person$nac_clas<-NULL

# Separamos la fecha de nacimiento
a_nac_s <- as.character(person$a_naci)
a_nac_y <- as.numeric(lapply(a_nac_s, function(x) substring(x,1,4)) )
a_nac_m <- as.numeric(lapply(a_nac_s, function(x) substring(x,5,6)) )

# Intervalo de edades a estudiar
edad_min<-20;
edad_max<-75;

# Grupos de edad
n_age_gr<- (edad_max-edad_min)/5

# Matriz que almacena el número de empleados por periodo (mes o quarter) y por edad
jf_i<- matrix(nrow=(edad_max-edad_min)/5,ncol=12); jf_i[,]=0;

# Matriz que almacena el numero de empleos destruidos
jd_i<- matrix(nrow=(edad_max-edad_min)/5,ncol=12); jd_i[,]=0;

# Un "truquito" para hacer matrices multidimensionales: lista de matrices:
# Para acceder a ellas: jf[[year]][edad,mes]
jf<- rep(list(jf_i),2012-2005+1)
jd<- rep(list(jd_i),2012-2005+1)



# Este loop va seleccionando año por año los contratos no destruidos antes de esta fecha
for (year in 2005:2012) {

	ptm <- proc.time()

	# En el primer fichero de "afiliad_i" vamos desde el individuo 1
	ID_i_1<-0

	# En cada uno de estos loops se debe recalcular la edad del individuo:
	# Calcular la edad de la persona. Para los fallecidos, la edad es "NA"
	#person$edad <- 2012 - (a_nac_y + a_nac_m/12)
	person$edad <- year - a_nac_y
	person$edad[person$f_fall != 0] <- NA; 


	# Loop que carga uno por uno los tres ficheros de afiliación
	for (i in 1:3) {

		ruta <- paste0(path_to_files,"MCVL2012AFILIAD",i,"_CDF.TXT")
	
		print(paste("Abriendo fichero ", i , sep="") )

		afiliad_i <- read.table(ruta,
		header=FALSE, sep=";", 
		col.names=c("ident_pf","reg_cot","gr_cotiz","tipo_con","coef","f_alta_r",
		"f_baja_r","causa_baja","gr_minus","cta_sec","dom_cta_sec","act_e_c09c","n_tr_cta",
		"f_alta_1_tr","TRL","col_trab","t_empl","t_empr_jur","noseque15","cta_prin","dom_soc",
		"f_con_1_m56","t_con_1_m56","c_con_1_m56",
		"f_con_2_m56","t_con_2_m56","c_con_2_m56",
		"fg_cot_1_m48", "gr_cot_1_m48", "act_ec_c93c") )

		# Para explorar los datos:
		#tail(afiliad1[,c(1,4,6,7,8,22,25)],10)
		#afiliad1[100:120,c(1,4,6,7,8,22,25)]

    # Descartar columnas innecesarias del data frame:
		afiliad_i$gr_mins<- NULL; afiliad_i$noseque15<-NULL; afiliad_i$dom_soc<-NULL
		afiliad_i$n_tr_cta<-NULL; afiliad_i$col_trab<-NULL; afiliad_i$act_ec_c93c<-NULL
		afiliad_i$f_con_1_m56<-NULL; afiliad_i$t_con_1_m56<-NULL; afiliad_i$c_con_1_m56<-NULL
		afiliad_i$f_con_2_m56<-NULL; afiliad_i$t_con_2_m56<-NULL; afiliad_i$c_con_2_m56<-NULL
#		afiliad_i$cta_prin <-NULL 
#		afiliad_i$cta_sec<-NULL
		afiliad_i$gr_minus<-NULL
		afiliad_i$pa_nac_cla<-NULL	
		afiliad_i$grcotiz<-NULL
		afiliad_i$t_empl<-NULL
		afiliad_i$t_empr_jur<-NULL

		# Esta variable elimina los contratos que ya han terminado antes de este año
		contr_termi<- as.numeric(paste(year,"0101",sep=""))

		# Esta variable da el código de los contratos NO CREADOS AÚN en el año correspondiente
		# (todos aquellos del año siguiente)
		contr_noempez<- as.numeric(paste(year+1,"0101",sep=""))

		# Y aquí cogemos el set relevante, dejándonos los contratos ACTIVOS
		print(paste("Subset fichero: ",i,", año: ", year,sep="")) 
		afiliad_i <- subset(afiliad_i, (f_baja_r>contr_termi)); 
		afiliad_i<-subset(afiliad_i,f_alta_r<contr_noempez) 



		# Obtener años, meses y días de las fechas de alta y baja
#		f_alta_s <- as.character(afiliad_i$f_alta_r); 
#		f_baja_s <- as.character(afiliad_i$f_baja_r);

#		afiliad_i$f_alta_y <- as.numeric(lapply(f_alta_s, function(x) substring(x,1,4)) )
#		afiliad_i$f_alta_m <- as.numeric(lapply(f_alta_s, function(x) substring(x,5,6)) )
#		f_alta_d <- as.numeric(lapply(f_alta_s, function(x) substring(x,7,8)) )

#		afiliad_i$f_baja_y <- as.numeric(lapply(f_baja_s, function(x) substring(x,1,4)) )
#		afiliad_i$f_baja_m <- as.numeric(lapply(f_baja_s, function(x) substring(x,5,6)) )
#		f_baja_d <- as.numeric(lapply(f_baja_s, function(x) substring(x,7,8)) )


		

	#	rm(f_alta_s); rm(f_baja_s)

		afiliad_i$f_alta <- as.Date(as.character(afiliad_i$f_alta_r),"%Y%m%d")
		afiliad_i$f_baja <- as.Date(as.character(afiliad_i$f_baja_r),"%Y%m%d")


		# La variable Tipo de Relación Laboral (TRL) identifica a los parados.
		afiliad_i$desem = as.numeric(afiliad_i$TRL==751 |afiliad_i$TRL==752 | afiliad_i$TRL==753 | 
		 afiliad_i$TRL==754 | afiliad_i$TRL==755 |afiliad_i$TRL==756)  


#		print(paste("Número de contratos fichero ",i,", año ",year,": ", dim(afiliad_i)[1],sep="") )


		print(paste("Fundiendo dataset ",i,", para contratos del año: ",year,sep="") )

		tot_i<-merge(person, afiliad_i, by="ident_pf")
	
	
		# Buscamos la destrucción de empleo por meses. Calculamos los que permanecen
		# trabajando a final de mes y los que no
		for (mes_i in 1:12) {

			if (mes_i<10) {mes_str=paste("0",mes_i,sep="")}else{mes_str=as.character(mes_i)}

#			print(paste("Mes ",mes_i,sep=""))

			# Vamos a crear la fecha de inicio del mes en cuestión:
			f_ref_alta <- as.Date(paste(year,mes_str,"01",sep=""),"%Y%m%d")
	
			print(f_ref_alta)

#			tot_i_f <- subset(tot_i, 
#			 (f_alta_y<year & f_baja_y==year & f_baja_m>mes_i & desem==0) |
#			 (f_alta_y<year & f_baja_y>year & desem==0 ) |
#			 (f_alta_y==year & f_baja_y==year & f_alta_m<=mes_i & f_baja_m>mes_i & desem==0) |
#			 (f_alta_y==year & f_baja_y>year & desem==0) )

#			tot_i_d <- subset(tot_i,
#			 (f_alta_y<year & f_baja_y==year & f_baja_m==mes_i & desem==0) |
#			 (f_alta_y==year & f_baja_y==year & f_alta_m<=mes_i & f_baja_m==mes_i & desem==0) )


			tot_i_f <- subset(tot_i, f_alta<f_ref_alta & (f_baja > (f_ref_alta+30) ) & desem==0 )
			tot_i_d <- subset(tot_i, 
			 f_alta<f_ref_alta & ( (f_baja>f_ref_alta) & f_baja <= (f_ref_alta+30) ) & desem==0 )
		

			edad_i<-edad_min
			ind_edad <- 1

			while (edad_i < edad_max) {
				tot_i_f_e <- subset(tot_i_f, (edad>=edad_i & edad<edad_i+5 ))
				tot_i_d_e <- subset(tot_i_d, (edad>=edad_i & edad<edad_i+5 ))
			
				jf[[year-2004]][ind_edad,mes_i] <- jf[[year-2004]][ind_edad,mes_i]+dim(tot_i_f_e)[1]
				jd[[year-2004]][ind_edad,mes_i] <- jd[[year-2004]][ind_edad,mes_i]+dim(tot_i_d_e)[1]


				edad_i<- edad_i+5
				ind_edad <- ind_edad+1

			} # EDAD

		} # FIN MES	

	} # FIN AFILIACION

	# Pon edad a cero para el siguiente loop:
	person$edad <-NULL

	ptm2 <- proc.time() -ptm
	print(ptm2)

} # FIN AÑO


# Calculamos las tasas de destrucción media mensual (para cada año)
# y con esto calcularemos la tasa promedio

jd_r<- matrix(nrow=(edad_max-edad_min)/5,ncol=12); jd_r[,]=0;
JD_R <- rep(list(jd_r),2012-2005+1)

tmp_d <- matrix( nrow=(edad_max-edad_min)/5,ncol=(2012-2005+1) ); tmp_d[,]=0;
#tmp_f <- matrix(n_row=(edad_max-edad_min)/5,2012-2005+1); tmp_f[,]=0;
mean_jd_r<- matrix(nrow=(edad_max-edad_min)/5,ncol=(2012-2005+1)); mean_jd_r[,]=0;

for (año in 2005:2012) {
	for (gr_edad in 1:n_age_gr) {
		for (mes_i in 1:12) {
			JD_R[[año-2004]][gr_edad,mes_i] <- jd[[año-2004]][gr_edad,mes_i]/
			(jf[[año-2004]][gr_edad,mes_i] + jd[[año-2004]][gr_edad,mes_i])
			
			tmp_d[gr_edad,año-2004] <- JD_R[[año-2004]][gr_edad,mes_i] + tmp_d[gr_edad,año-2004]
		}

		mean_jd_r[gr_edad,año-2004] <- tmp_d[gr_edad,año-2004]/12

	}
	
}


# The AVERAGE MONTHLY job destruction for the years 2005-2012 is:
MEAN_JD_R <- apply(mean_jd_r,1,mean)


png("/home/adrian/Documentos/data/MCVL/Flujos/jd_age_2.png", width=700, height=700)
barplot(MEAN_JD_R[1:9],
ylim=c(0,.15),
main="Monthly destruction rates by age (mean period 2005-2012)",
names.arg=c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59", "60-64"),
col="lightgray",
xlab="Age cohorts",
axis.lty=1)
dev.off()

# Guardar datos agregados por meses y años
#write.table(jd,paste0(path_to_output, JD.txt"))
#write.table(jf,paste0(path_to_output, JF.txt"))

# Guardar todo
save(jd,jf,JD_R,MEAN_JD_R,file=paste0(path_to_output,"JDF.RData"))

# Para cargar estos dos objetos, usaremos load("JDF.RData")


stop()

# Ahora calculamos la destrucción de empleo por trimestre

jd_q <- matrix(nrow=n_age_gr,ncol=4) 
jf_q <- matrix(nrow=n_age_gr,ncol=4)
jd_r_q <-matrix(nrow=n_age_gr,ncol=4)




for (age_gr in 1:n_age_gr) {
	quarter<-1
	ind_q<-1
	while (quarter<=4) {
		jf_q[age_gr,quarter] <- sum(jf_i[age_gr,ind_q:(ind_q+2)])
		jd_q[age_gr,quarter] <- sum(jd_i[age_gr,ind_q:(ind_q+2)])

		jd_r_q[age_gr,quarter] <- jd_q[age_gr,quarter]/jf_q[age_gr,quarter]
		
		ind_q=ind_q+3; quarter= quarter+1
	}
}

# Y calculamos el promedio por trimestre (para cada edad
jd_r_q_av<- vector(mode="numeric")

for (age_gr in 1:n_age_gr) {
	jd_r_q_av[age_gr]<- mean(jd_r_q[age_gr,1:4])
}

# Exportar los datos
write.table(jd_r_q_av,"jd_r_q_av.txt", sep=",")


# Hacemos un bonito gráfico
png("jd_age.png", width=600, height=600)
barplot(jd_r_q_av[1:9], 
main="Average quarterly job destruction rates (2012)",
ylim=c(0,.3),
names.arg=c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59", "60-64"),
col="lightgray",
xlab="Age cohorts",
axis.lty=1)
dev.off()
