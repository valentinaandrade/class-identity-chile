Methods

Woooldrige (2013, cap. 17 p.579)
 - Desventajas más importantes de LPM son:
   - Probabilidades ajustadas pueden ser menores que cero o mayores que uno
   - Efecto parcial de cualquier variable explicativa es constante.

   Si la experanza de E(y|x) no es lineal, MCO y MCP no son aplicables
   Ajuste en LPM puede ser interpretado como porcentaje de la varianza correctamente predicha, pero puede obtenerse procentajes muy altos con precisión aun cuando el resultado es menos probableesté preicho de manera deficiente. **Cuando un resultado es poco probable el R^2 se distorciona**.
   - Por eso en logistica se ocupa Pseudo -R cuadrado

   Argumentos a favor del modelo lineal
   - En Frangi et al. 2017 se realiza principalmente por las interacciones. Se estiman los modelos con una iteración de los errores (metodo para estimar LPM) y luego se ve que las estimacion de las interacciones proveen un n de observaciones que son lo suficientemente grandes para producir un comporamiento asintótico a la OLS
   - Las estimaciones logit estan muy influenciadas por las variables no observadas (Mood, 2010).
   - Se mencionan otros problemas relativos a la comparación de modelos anidados y submuestras de estos, pero que no aplica para nuestro caso.
   - Se deben ocupar errores estándar robustos (estimación por iteración)

   Dudas:
   - Nivel 1 hace estimaciones sin diferenciar países( efectos fijos)
   - Nivel 2 contextuales incorpora e.Fijos

Encontra de LPM:
- Tamaño efecto clases sobre identidad de clases
- Dirección/tamaño coeficientes de año (igual)
- Ajuste global bajo


Generales:
- ¿Porque cuando incorporamos interaccion año se vuelve significativa?
