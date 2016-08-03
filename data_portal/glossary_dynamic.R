#actionButton("show","Show"),
#actionButton("hide", "Hide"),
#hr(),
bsCollapse(id = "collapseGlossary", open="GLOSSARY",
bsCollapsePanel("GLOSSARY",
                style="info",
                p(em("Look up definitions for intervention, outcome, biome, and study design types used to categorize this evidence map. We use the the IUCN-Conservation Measures Partnership standardized typology for conservation actions/intervention types and WWF ecoregion definitions and spatial mapping for all biome types. Included specific actions are listed for each type. The outcome categories were synthesized from a number of commonly used sources. Study design definitions are modeled after ones employed by REFERENCE (see ",span(strong("ABOUT"),style="color:#006699")),"for more information)"),
                column(6,
                       selectInput("intervention",
                                   label = "Intervention type:",
                                   choices= list("Area protection", "Resource protection/management","Land/Water management",  "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building")
                       ),
                       p(div(strong("Definition:"))),
                       textOutput("int_def2"),
                       p(div(strong("Included Actions:"))),
                       textOutput("int_def3")
                ),
                column(6,
                       selectInput("outcome",
                                   label = "Human well-being outcome:",
                                   choices = list("Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Cultural & Spiritual", "Freedom of choice/action")
                       ),
                       p(div(strong("Definition:"))),
                       textOutput("out_def2"),
                       p(div(strong("Examples:"))),
                       textOutput("out_def3")
                )
                
)