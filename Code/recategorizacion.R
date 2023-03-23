Honduras_final_2022 <- read_dta("~/OneDrive - World Justice Project/INL WH Expanded GPP/Year Two - 2022/3. Data Collection/Fieldwork/Final Data/Mercaplan/Honduras/Merge/Honduras_final_2022.dta")

recategorization <- Honduras_final_2022 %>%
  select(Q11E_1_OTH, EXP_q8h, Q11E_2_OTH) %>%
  mutate(EXP_q8h = as.numeric(EXP_q8h),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("No supe a dónde acudir/a quién contactar")),
                                  1, EXP_q8h, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("No pensé que me creyeran|
                                                    PORQUE ERA UNA MUJER")), 
                                  2, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("TUVE ASUSTADA|
                                                    Tuvo miedo de ir a la policía|
                                                    NO TENIA PAPELES|
                                                    Miedo|
                                                    La persona la amenazó|
                                                    La amenazaron porque piensa diferente a ellos|
                                                    Es una situación política|
                                                    Tuve miedo de represalias o temía por mi seguridad")), 
                                  3, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("Fue un familiar")), 
                                  4, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("No pensó que fuera a mejorar la situación|
                                                    No pensó que fuera necesario|
                                                    No supe quien se las llevo|
                                                    El hombre corrió y no le pudimos ver la cara|
                                                    FALTA DE EVIDENCIA|
                                                    Ha sido en otros barrios|
                                                    NO LO MIRO MUY GRAVE|
                                                    No era grave|
                                                    No era muy valioso lo que sustrajeron|
                                                    No eran de mucha importancia lo robado y no contaba con las suficientes pruebas|
                                                    No había prueba no había sospechoso|
                                                    No quería problemas|
                                                    No quería q se fuera a mayores cosas|
                                                    No quizo|
                                                    No recordaba a la persona|
                                                    No sabía quien fue|
                                                    No supo quién era la persona|
                                                    No toma acción, a muchas personas les roban y toman importancia|
                                                    Pagaron el daño|
                                                    Para evitar problemas en la familia|
                                                    Porque no se dio cuenta, no supo quien le robo|
                                                    Ya estaba la gente en el lugar|
                                                    enfrento personalmente|
                                                    no hacen nada|
                                                    no le dan mucha importancia|
                                                    no tenia evidencia|
                                                    pensó que el robo no fue mayor|
                                                    porque no paso a mas|
                                                    propios medios|
                                                    simplemente  lo dej o pasar|
                                                    todos seconocen")), 
                                  5, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("Un amigo o familiar me convenció de no hacerlo")), 
                                  6, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("No pensó que la policía u otra autoridad fueran a hacer algo al respecto|
                                                    La policía no funciona|
                                                    NUNCA HACEN NADA|
                                                    No había personal policial suficiente ese día|
                                                    No hacen nada|
                                                    No hacen nada los policías|
                                                    No hacen nada no actúan|
                                                    No hacen nada y ellos son los mismos delincuentes|
                                                    No hacen nada y uno pierde el tiempo|
                                                    Por la pobreza no le hacen caso|
                                                    Porque no hacen nada")), 
                                  7, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("No tuve tiempo|
                                                    DISTANCIA|
                                                    Familiar hospitalizado|
                                                    Iba de prisa|
                                                    Lo reportó a los derechos humanos|
                                                    No había autoridades cerca|
                                                    Porque tenía que viajar|
                                                    Pérdida de tiempo|
                                                    estaba trabajndo|
                                                    lejania geografica|
                                                    ")), 
                                  8, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_2_OTH, 
                                             regex("No tuve tiempo")), 
                                  8, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("Me defendí")), 
                                  9, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("Fueron los policías los que hicieron el delito|
                                                    Desconfianza en la policía u otras autoridades|
                                                    Estaban cerca agente de policía|
                                                    No hay una regulación de la institución encargada|
                                                    PORQUE SI VA LE PIDEN DINERO PARA PODER RESOLVER EL PROBLEMA|
                                                    Son los de los de la misma policía no se puede denunciar")), 
                                  10, EXP_q8h_prueba, NA_real_),
         EXP_q8h_prueba = if_else(str_detect(Q11E_1_OTH, 
                                             regex("Estaba avergonzado")), 
                                  11, EXP_q8h_prueba, NA_real_))

recategorization <- Honduras_final_2022 %>%
  select(Q26D_9_1_OTH, Q26D_9_2_OTH, Q26D_9_3_OTH) %>%
  mutate()
  
         