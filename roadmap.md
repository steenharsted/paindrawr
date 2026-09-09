# Roadmap

# Main programmer on functions:

* Both SFH + SON
    - helper_functions.R
* SFH:
    - pdr_add_rgba.R
    - pdr_alpha.R
    - pdr_heatmap.R
    - pdr_recreate_drawing.R
* SON
    - pdr_check_data.R
    - pdr_explode.R
    - pdr_implode.R
    - pdr_import_json.R
    - pdr_import_mird.R
    - pdr_poly_anatomy_overlap.R **DEFUNCT -- TO BE REMOVED**
    - pdr_poly_areas.R
    - pdr_poly_manage_overlaps.R **DEFUNCT -- NOW IN HELPER FUNCS**
    - pdr_sanitize.R
    - pdr_spray_trim.R **SHOULD BE RGBA_TRIM**
    - pdr_spray_areas.R **DEFUNCT -- REMOVE**


## Convert from old to new pd data structure:

Old structure: a tibble with list-col's 's' and 'p'
New structure: a list of lists (one for each pain drawing)

New names in pain drawing definition:
* .id
* .file
* .version
* .width
* .height
* .units
* .timestamp
* .app
* .strokes
    - .index
    - .tool
    - .tool_width
    - .color
    - .alpha
    - .zoom
    - .zoom_width
    - .zoom_height
* .points
    - .index
    - .x
    - .y

Convert to new structure (mark with DONE, when converted and working)?
* Both SFH + SON
    - helper_functions.R
    - DOCUMENTATION !!
    
* SFH:
    - pdr_add_rgba.R
    - pdr_alpha.R
    - **DONE** pdr_heatmap.R
    - **DONE** pdr_recreate_drawing.R
    - pdr_spray_areas.R **SHOULD BE SPRAY_RGBA**
* SON
    - **DONE** DATASET.R 
    - **DONE** pdr_check_data.R
    - **DONE** pdr_explode.R
    - **DONE** pdr_implode.R
    - **DONE** pdr_import_json.R
    - pdr_import_mird.R --- this function expects a string - not a filename
    - **DONE** pdr_poly_areas.R
    - **DONE** pdr_modify.R operations
    - add operation to modify to 'reduce_to_templates'
    - pdr_spray_trim.R **SHOULD BE RGBA_TRIM**
    - **DONE** pdr_get_info.R
    - pdr_subtract_template -- fjern in/out ift skabelon
    - pdr_get_summary -- n strokes, n points, polyarea, alphaarea, alphaintensity, tool, ?



## To do

- pd_poly_manage_overlaps ... should it be a hidden function? ... it is called by the pd_sanitize function
    * YES! Vi gør den skjult for brugerne
- i pd_json2pd .. skal vi flippe y aksen? skal det være en option?
    * YES! Vi gør det til en funktions parameter med default = TRUE (flip)
- kunne pd_recreate_drawing plotte alle på samme baggrund i stedet for flere facetter?
    * NO! ...funktionen hedder 're-create' .. skal derfor lave original indtastningen
- vi skal kigge nærmere ind i pd_json2pd funktionen med nogle ægte PainSketchR data -- 's' kolonnen, default w og h værdier?
- ændre pd_spray_trim til pd_png_trim
    * YES! Omdøb til pdr_rgba_trim
- i pd_png_trim funktionen kan vi droppe alt om fil-indlæsning og i stedet bruge .png kolonner genereret af Steens pd_to_png funktionen
    * YES! Det skal SON fikse og husk: OBS navne ændret til rgba
- i pd_png_trim funktionen skal vi indføre en parameter i funktionskaldet 'col_name' som definerer kolonne navnet i pd data hvor den nye (trimmede) png data indsættes
    * NO! Vi laver samme funktionalitet som i pdr_sanitize, hvor den kan håndtere kolonne ṕ' OG et helt vaid pd tibble
- i sammenfattende funktioner (f.eks pd_anatomy_merge) hvor et antal tegninger/række bliver 'summarized' til én skal vi sætte 's' kolonnen til fornuftige default værdier
    * YES! Det har SON lavet -- skal lige tjekkes så ned i DONE
- hvordan skal vi håndtere NA værdier -- f.eks hvis 's' og 'p' er NA? (pdr_check_data)
    * NA værdier er acceptable i kolonnerne s, p, coord, ts, app
    * NA værdier er IKKE acceptable i kolonnerne id, w, h
- pd_mird2pd skal generere fornuftige (og korrekte) default værdier for w, h, coord, etc
    * YES! Det skal vi bare have Natalie til at hjælpe med ... de rigtige værdier indsættes i import funktionen
- Tjek korrekt brug af inst/extdata til eksterne data sæt som brugeren kan læse ind med import funktionerne
    * SON !
- pd_demodata skal indeholde to kategoriske og 1 kontinuerlig variabel til demonstration af pd_create_heatmap
    * SON ! F.eks Group: A|B|C, Treatment: Active|Placebo og VAS: (..en moduleret pd area)
- webpage
    * Hvornår er vores dokumentation god nok til at prøve at generere en info page?
- gitlab SDU?
    * Skal vi flytte repo til gitlab MAYBE! Det tænker SFH over ....
- alle funktioner nødvendige?
    * SON + SFH kigger lige funktions listen igennem -- har vi det (og kun det) vi skal bruge?
- functioner som kalder sig selv re-kursivt, f.eks pdr_sanitize vil give en Warning (fra pdr_cheack_data) ... what to do?
- skriv en 'get_pd_id' funktion til at trække id ud fra smertetegninger -- eller bredere: en pdr_summary function 
- i pdr_check_data -- skal vi lave en bruger service hvis de bruger en 'list of named variable' i stedet for en 'list of lists of...' -- altså de har indsendt en enkelt pd, ikke en liste af pd

## Done

- SON: pdr_check_data skal tjekke indholdet af p og s, højde og bredde m.m.
- SON: Warning hvis flere farver 
- SH: summary af .png (intensitet / alpha)
- SH: ændret pd_add_png() til pd_to_png() den virker nu via mutate(). pd_add_png() er fjernet
- SH: ændret pd_alpha_intensity() og pd_alpha_area() således at de nu vikrer via mutate() uden at bruger skal wrappe med map_dbl()
- SH: - kan png filer skabes i ram uden at skrive til disk? Ja - indført som default
- SH: - skal a i kolonne s i pain drawing data struktur være 0:1 eller 0:255 eller 0:127 Yes - er 0:255