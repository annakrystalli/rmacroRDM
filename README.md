# rmacroRDM
<br>

#### **Please note: Project under construction**

### Tools to handle trait macroecological datasets in R


Trait databases are all the rage these days, for good reason. Traits are interesting from evolutionary and ecological perspectives. While much progress is being made in compiling and making centralised datasets programmatically accessible, often macroecologists at the level of individual researcher or lab, still spent a lot of time manually compiling datasets scrapped from pds, literature searched and entered into excel spreadsheets.

As the size and diversity of datasets increases, so does the requirement for data management to ensure good tracking of data provencance and specification of data key. Efforts to standardise by promoting metadata standards. eg in ecology a number eg EML and availability in R there. 

So the responsibility however lies on researchers which often lack the time, skill, resources or tools needed to achieve good data management practices. 

<br>

***

## **`rmacroRDM`: macroecological data management package in R**

Both the last two projects I have been working on involved compiling datasets into large master trait dataset. The basic premise has been pretty consistent. Here's a master data sheet, here's bunch more data in various formats and with varied reference information. Here's some more open sources of data Put it all together and prepare it for for analysis. So I've ended up with quite a bit of functional code and a somewhat developed framework to see how a better developed package could really provide useful functionality.

I'm hoping to compile the code into a package to facilitate data management and standardisation of macroecological trait datasets. Good data and metadata management making it easier to add to, subset, query, visualise analyse and share.



### **Proposed functionality:**
 
#### see [**vignette**](https://github.com/annakrystalli/rmacroRDM/blob/master/vignette.md) for a demo of current functionality

<br>

#### Matching and tracking taxonomy metadata


- integrate **`taxize`**
  - auto correct species names input errors with fuzzy matching
  - 
- network of synonyms


#### Matching and tracking observation metadata

- QC
- observer details
- synonym version control
- network of citations to track duplication though reuse of data

#### Matching and tracking variable metadata

- enforce complete metadata to add variables. Check consistency of units before adding data.
- metadata readily available for plotting and extraction for publication.


#### Basic Quality control functionality
- some simple tools to help identify errors, outliers etc

#### Produce analytical datasets

Package to include functions that allow users to:

- interrogate database, extract some information on data avalaibility (partcularly complete cases resulting from different combination of variables).
- Allow to specify taxonomic and variable subsets
- Produce **wide analytical dataset**. Contain a selection of functions to summarise duplicate datapoints according to data type. 
    - output to include variable subset metadata
    - a list of all references used to create analytical dataset.


This could  be further extended to further  exploring potential biases.
        - taxonomic biases (ie calculate taxonomic distinctness of subsets of complete case species for different variable combinations)
        - data gap biases  
        - basic covariance structure between variables. Could be used to relate to data gaps to understand how missing values might affect results.
 

<br>

***

## **package details**

### **data format**

#### Structure it like a mini database. 
The idea is to mantain a master database to which new datasets are added.

#### Structure would consist of:

<br>

- **long data point table:** each row a data point. columns consist of data point metadata, eg.:

    + species 
    + variable
    + value
    + taxonomic matching info
    + original reference
    + observer: if manually mined form literature. Could also describe scrapping procedure
    + quality control (ie confidence in observation)
    + n: if value derived from multiple measurements, number of observations
    + dataset code
    + etc
    
- **master species list:**: species list to which all data are to be matched


- **taxonomic table:**: taxonomic data for each species used in

    + perhaps even a tree?
    
- **variable metadata table:** each row a variable: columns:

    + code: variable short code to use throughout database 
    + type: eg. continuous, integer, nominal, categorical, binary, proportion etc
    + var category: eg. ecological, morphological, life history, reproductive etc.
    + descr: longer description for plotting
    + units
    + scores: used for factor/categorical/binary variables in data
    + levels: if variable is factor/categorical/binary
    + method code
    
- **cross-dataset variable name table:** each row a variable in the master: 
    - columns are the names of corresponding variables across different datasets
    
- **method table:** each row a method referenced in variable metadata:
    
    + method code
    + method description etc
    
    
    
    
    
