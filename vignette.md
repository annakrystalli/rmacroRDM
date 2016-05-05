The rmacroRDM package contains functions to help with the compilation of
macroecological datasets. It compiles datasets into a **master long
database of individual observations**, matched to a specified **master
species list**. It also *checks, separates and stores taxonomic and
metadata information* on the *observations*, *variables* and *datasets*
contained in the data. It therefore aims to ensure full traceability of
datapoints and as robust quality control, all the way through to the
extracted analytical datasets.

The idea is to enforce a basic level of data management and quality
control and bundling it with important metadata for both each stage in
data processing and compilation and **`[[master]]` outputs**. Managing
data in such a way makes validating, understanding, analysing,
visualising and communicating much easier.

Standardisation allows data to be shared and build upon more easily and
with higher robustness. It also allows more interactivity as apps can be
built around **rmacroRDM data outputs** to facilitate data exploration,
validation, access and reporting. It also allows data to be shared and
build upon more easily and with higher robustness.

### **rmacroRDM `[master]` dataset**

The overall purpose of the functions in the package are to compile
macroecological trait datasets into **a master database of observations
(T1)**. This allows information to be stored with individual datapoints,
allowing for better quality control and traceability.

Metadata information on individual data points stored in the **long
`master` dataset** is defined by assigning ***observation metavariables
`{meta.vars}`***. Information on the taxonomic matching of datapoints
through synonyms is also stored and is defined through **match variables
`{match.vars}`**. In the example below, "*species*", *var*", "*value*"
identify each oservation, "*data.status*" "*qc*","*observer*", "*ref*"
and "*n*" are the default **`meta.vars`** and "*synonyms*" and
"*data.status*" are the default **`match.var`**.

<table>
<caption>T1: example master data sheet</caption>
<thead>
<tr class="header">
<th align="left">species</th>
<th align="left">var</th>
<th align="right">value</th>
<th align="left">data</th>
<th align="left">synonyms</th>
<th align="left">data.status</th>
<th align="left">qc</th>
<th align="left">observer</th>
<th align="left">ref</th>
<th align="right">n</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Acanthis_flammea</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Acanthis_flammea</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Healy, Kev ...</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">Acanthis_hornemanni</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Acanthis_hornemanni</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Healy, Kev ...</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">Acanthisitta_chloris</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Acanthisitta_chloris</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_badius</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Accipiter_badius</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">Accipiter_brevipes</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Accipiter_brevipes</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_cooperii</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Accipiter_cooperii</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">Accipiter_gentilis</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Accipiter_gentilis</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_melanoleucus</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">D0</td>
<td align="left">Accipiter_melanoleucus</td>
<td align="left">original</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="right">NA</td>
</tr>
</tbody>
</table>

This framework also can handle multiple intraspecific datapoints for
individual `{vars}` allowing users to build up information of trait
intraspecific variation across observations. Observation metadata
enables quality control to filter data supplied to analytical datasets.

------------------------------------------------------------------------

<br>

match objects {`m`}
-------------------

Functions in the rmacroRDM package have been designed to receive and
update a match object (**`m`**). This helps keep all the information
relating to the matching of a particular dataset together, updatwed at
the same time and available and updated at each stage.

Match objects `[[m]]` are defined by the function **`matchObj()`**

     m <- matchObj(data.ID, spp.list, data, status = "unmatched", 
                   sub = "data", meta, filename)

and have the following elements:

    ## [1] "data.ID"  "spp.list" "data"     "sub"      "set"      "status"  
    ## [7] "meta"     "filename"

------------------------------------------------------------------------

### **`[[m]]` structure**

#### **`"data.ID"`**

a character vector of the dataset code *(eg. `"D1"`)*

#### **`[spp.list]`**

dataframe containing the master species list to which all datasets are
to be matched. It also tracts any additions (if allowed) during the
matching process.

#### **`[data]`**

dataframe containing the dataset to be added

#### **`"sub"`**

character string, either `"spp.list"` or `"data"`. Specifies which
`[[m]]` element contains the smaller set of species. Unmatched species
in the subset are attempted to be matched through synonyms to `[[m]]`
element datapoints in the larger species set.

#### `"set"`

character string, either `"spp.list"` or `"data"`. Specifies which
`[[m]]` element contains the larger set of species. Automatically
determined from `m$sub`.

#### `"status"`

character string. Records status of `[[m]]` within the match process.
varies between
`{"unmatched", "full_match", "incomplete_match: (n unmatched)"}`

#### **`[[meta]]`**

list, length of `{meta.vars}` containing observation metadata for each
`"meta.var"`. `meta$meta.var` can be a `[dataframe]` or
`"character_string"`. The default `meta.vars` represent observation
metadata commonly associated with macroecological datasets. `NULL`
elements are added as NA

-   `"ref"`: the reference from which observation has been sourced. This
    is the only `meta.var` that *MUST* be correctly supplied for
    matching to proceed.
-   `"qc"`: any quality control information regarding
    individual datapoints. Ideally, a consistent scoring system used
    across compiled datasets.
-   `"observer"`: The name of the observer of data points. Used to allow
    assessment of observer bias, particularly in the case of data
    sourced manually form literature.
-   `"n"`: if value is based on a summary of multiple observations, the
    number of original observations value is based on.
-   `"notes"`: any notes associated with individual observations.

#### **`"filename"`**

character string, name of the dataset filename. Using the filename
consistently throughout the file system enables automating sourcing of
data.

#### **`"unmatched"`**

stored details of unmatched species if species matching incomplete.

------------------------------------------------------------------------

<br>

additional `[metadata]`
-----------------------

### - variable `[metadata]`

Metadata on `{vars}` are stored on a separate sheet. Completeness of the
metadata sheet is not only checked for but it also required for many of
the functions. It's also extremely useful downstream, at data analysis
and presentation stages.

**`[metadata]`** contains information on coded variables. Typical
information includes:

-   **`desc`**: a longer description of vars,
-   **`cat`**: var category
-   **`units`**
-   **`type`**:

    -   `"bin"`: binary
    -   `"cat"`: categorical
    -   `"con"`: continuous
    -   `"int"`: integer
-   **`scores`** & **`levels`** if the variable is categorical `"cat"`
    or binary `"bin"`.
-   **`notes`** any textual information supplied with the data.
-   **`log`** `T` or `F`. Often useful to be able to assign whether a
    variable should be logged for exploration and analysis.

<table>
<caption>T2: example variable metadata sheet</caption>
<thead>
<tr class="header">
<th align="left"></th>
<th align="left">code</th>
<th align="left">cat</th>
<th align="left">descr</th>
<th align="left">scores</th>
<th align="left">levels</th>
<th align="left">type</th>
<th align="left">units</th>
<th align="left">notes</th>
<th align="left">log</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">2</td>
<td align="left">bite</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Species responds to being touched on the beak with biting</td>
<td align="left">0;1</td>
<td align="left">FALSE;TRUE</td>
<td align="left">Bin</td>
<td align="left"></td>
<td align="left">If the bird bite when researchers put the index finger of the right hand against the beak. 0: not bite, 1: bite</td>
<td align="left">FALSE</td>
</tr>
<tr class="even">
<td align="left">4</td>
<td align="left">brood.par</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Percentage of cuckoo parasitized nests of a given host species</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left">%</td>
<td align="left"></td>
<td align="left">FALSE</td>
</tr>
<tr class="odd">
<td align="left">5</td>
<td align="left">coloniality</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Coloniality</td>
<td align="left">0;1;2</td>
<td align="left">Solitary;Colonial;Semicolonial</td>
<td align="left">Cat</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">FALSE</td>
</tr>
<tr class="even">
<td align="left">6</td>
<td align="left">colony.size</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Maximum colony size</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Int</td>
<td align="left"></td>
<td align="left">transformed toa a log -10 scale; 0: Solitary to 5: Colony size exceeding 100.000</td>
<td align="left">FALSE</td>
</tr>
<tr class="odd">
<td align="left">7</td>
<td align="left">disperse.dist</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Maximum dispersal distance</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left">km</td>
<td align="left">the minimum distance from the mainland to an island with a permanent breeding population</td>
<td align="left">TRUE</td>
</tr>
<tr class="even">
<td align="left">8</td>
<td align="left">diving</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Species dives</td>
<td align="left">0;1</td>
<td align="left">FALSE;TRUE</td>
<td align="left">Bin</td>
<td align="left"></td>
<td align="left">1:Diving, 0:No diving</td>
<td align="left">FALSE</td>
</tr>
<tr class="odd">
<td align="left">9</td>
<td align="left">egg.reject</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Rejection rate of parasitic eggs laid in the host nests</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left">%</td>
<td align="left"></td>
<td align="left">FALSE</td>
</tr>
<tr class="even">
<td align="left">10</td>
<td align="left">fear.scream</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Fear scream score during handling (0-1)</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left"></td>
<td align="left">While the bird was handled, if it have a fear scream score of 1, if not score of 0</td>
<td align="left">FALSE</td>
</tr>
</tbody>
</table>

### - \[syn.links\]

Another important aspect of matching macroecological variables is using
know synonym links across taxonomies to match species names across
datasets. Synonyms are su two data column containing unique pairs of
synonyms.

In my example, I provide `[syn.links]` which **contains unique synonym
links I have compiled** throughout the projects I've worked on and
**only pertains to birds**.

This is by no means complete and often, some manual matching (eg through
[avibase]() is required. The hope is to integrate rmacroRDM with a
package like [`taxise`](), linking the process to official repositories,
automating as much as possible and enabling better tracking of the
network of synonyms through taxonomies used to match species across
datasets. [**ISSUE**]()

    ##                synonyms                species
    ## 1  Pteroicnemia_pennata    Pterocnemia_pennata
    ## 2    Casuarius_casurius    Casuarius_casuarius
    ## 3        Apteryx_haasti        Apteryx_haastii
    ## 4         Apteryx_oweni         Apteryx_owenii
    ## 5       Nothura_darwini       Nothura_darwinii
    ## 6       Toaniscus_nanus        Taoniscus_nanus
    ## 7 Pygoscelis_antarctica Pygoscelis_antarcticus
    ## 8    Eudyptes_crestatus    Eudyptes_chrysocome

------------------------------------------------------------------------

<br>

{file.system} management
========================

Many of the functions in the rmacroRDM package are set up to allow for
automatic loading and processing of data from appropriately named
folders. This allows quick and consistent processing of data. However it
does depend on data and metadata being correctly labelled and saved in
the appropriate folders. This tutorial will guide you through correct
setup and walk through an example of adding a dataset to a master macro
dataset.

<br>

**setup**
---------

The first thing to do is to specify the input, output and script
folders, set up the data input folder and populate it with the
appropriately named data in the appropriate folders.

### settings

    ### SETTINGS ##############################################################

    options(stringsAsFactors = F)

    output.folder <- "~/Documents/workflows/rmacroRDM/data/output/"
    input.folder <- "~/Documents/workflows/rmacroRDM/data/input/"
    script.folder <- "~/Documents/workflows/rmacroRDM/R/"

### **setup `input.folder`**

Once initial settings have been made, you will need to setup the input
folder. The easiest way to do this is to use the
**`setupInputFolder()`**. This defaults to meta.vars: `"qc"`,
`"observer"`, `"ref"`, `"n"`, `"notes"`. The function is however
flexible so the meta.vars can be customised to meet users observation
metadata needs. The basic folders created by the function are:

    # custom meta.variables can be assigned by supplying a vector of character strings to the metadata
    # argument in function setupInputFolder()

    meta.vars = c("qc", "observer", "ref", "n", "notes")

    setupInputFolder(input.folder, meta.vars)

**Populate input folders**
--------------------------

<br>

### **data**

-   **`raw/`** : a folder to collect all raw data. These files are to be
    treated as *read only*.
-   **`csv/`** : raw data files should be saved as .csv files in
    this folder. This is the folder from which most functions will
    source data to be compiled.

#### **`csv/`**

Because it is the most common form encountered, data in the `csv` folder
is usually given in a wide format (ie species rows and variable
columns). For traceability, is is good practice to name the `.csv` files
as the **original raw data file name** from which they were created.

-   eg. in our example the datasheet being added and saved in folder
    `csv/` as **`D1.csv`**. Any meta.var data associated with this
    dataset should also be saved in the appropriate meta.var folder
    **`D1.csv`**.

##### Pre-processing

Some pre-processing might be required. In particular, the column
containing species data should be labelled **`species`**. In a
pre-processing stage, you might need to match **master `code`** and
**data** variable names representing the same variable. I recommend this
be done in a scripted pre-processing step using a variable lookup table
to keep track of raw variable names across data sets. eg

<table>
<caption>Correspondence of D0 &amp; D1 dataset variable names to master variable codes</caption>
<thead>
<tr class="header">
<th align="left"></th>
<th align="left">code</th>
<th align="left">D0</th>
<th align="left">D1</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">22</td>
<td align="left">birth.wgt</td>
<td align="left">Birth weight</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">23</td>
<td align="left">bite</td>
<td align="left">Biting</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">24</td>
<td align="left">bmr</td>
<td align="left">BMR</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">25</td>
<td align="left">body.len</td>
<td align="left">Body length</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">26</td>
<td align="left">body.mass</td>
<td align="left">Body mass</td>
<td align="left">adult_body_mass_g</td>
</tr>
<tr class="even">
<td align="left">27</td>
<td align="left">body.mass.f</td>
<td align="left">Body mass: Females</td>
<td align="left">female_body_mass_g</td>
</tr>
<tr class="odd">
<td align="left">28</td>
<td align="left">body.mass.m</td>
<td align="left">Body mass: Males</td>
<td align="left">male_body_mass_g</td>
</tr>
<tr class="even">
<td align="left">29</td>
<td align="left">branchial.id</td>
<td align="left">Brachial index</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">30</td>
<td align="left">brain.mass</td>
<td align="left">Brain mass</td>
<td align="left"></td>
</tr>
</tbody>
</table>

If there are meta.var data included in the dataset, these should be
labelled or appended with the appropriate meta.var suffix, (*eg. `ref`
if meta.var* **ref** *relate to all variables or `body.mass_ref` if
meta.var* **ref** *variable refers to a particular variable, in this
case body mass.*) Correct naming of meta.var columns will allow
**`separateMeta()`** to identify and extract meta.var data from the
dataset. Also ensure taxonomic variables (*eg. class, order etc*) are
removed from data to be added. So datasets should contain a `species`
column, any *`variable`* data columns to be added and can additionally
contain appropriately appended *`meta.var`* columns.

<br>

**{meta.vars}**
---------------

### Supplying meta.vars.

Meta.vars can either be supplied directly to the appropriate functions
by attaching to the appropriate element of the **`meta` list object**
or, data can be saved in appropriately named folders. Files should be
named the same as the data sheet being compiled.

### meta.var data formats

There are a number of formats meta variable data can be supplied in.

<br>

##### **single value across all species and variables**

If a **single value relates to all data** in the data file (eg all data
sourced from a single reference), then meta.var can be supplied as a
**single value or character string** (eg a character string of the
reference from which the data has been sourced).

<br>

##### **single value across all variables, but not species**

If a **single value** relates to **all variables** in the data but
**varies across species**, metavariable data should be supplied as a two
column dataframe with columns named `species` and `all`, eg:

<table>
<caption>Example ref meta.var data where reference is same across variables but varies across species</caption>
<thead>
<tr class="header">
<th align="left">species</th>
<th align="left">all</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Acanthisitta_chloris</td>
<td align="left">Higgins, P.J., Peter, J.M. &amp; Steele, W.K. (eds) 2001. Handbook of Australian, New Zealand and Antarctic Birds. Volume 5: Tyrant-flycatchers to Chats. Oxford University Press, Melbourne.</td>
</tr>
<tr class="even">
<td align="left">Acanthiza_lineata</td>
<td align="left">Higgins, P.J. &amp; Peter J.M. (eds) 2002. Handbook of Australian, New Zealand and Antarctic Birds. Volume 6: Pardalotes to Shrike-thrushes. Oxford University Press, Melbourne</td>
</tr>
<tr class="odd">
<td align="left">Acanthiza_pusilla</td>
<td align="left">Higgins, P.J. &amp; Peter J.M. (eds) 2002. Handbook of Australian, New Zealand and Antarctic Birds. Volume 6: Pardalotes to Shrike-thrushes. Oxford University Press, Melbourne</td>
</tr>
<tr class="even">
<td align="left">Acanthiza_reguloides</td>
<td align="left">Higgins, P.J. &amp; Peter J.M. (eds) 2002. Handbook of Australian, New Zealand and Antarctic Birds. Volume 6: Pardalotes to Shrike-thrushes. Oxford University Press, Melbourne</td>
</tr>
<tr class="odd">
<td align="left">Acanthorhynchus_tenuirostris</td>
<td align="left">Higgins, P.J., Peter J.M. &amp; Steele W.K. (eds) 2001. Handbook of Australian, New Zealand and Antarctic Birds. Volume 5: Tyrant-flycatchers to Chats. Oxford University Press, Melbourne</td>
</tr>
</tbody>
</table>

<br>

##### **Value varies across variables and species**

There are two ways meta.var data that vary across species can be
formatted. The simplest is a **species** x **var** dataframe where
meta.var columns relating to specific variables are named according to
the variables in the data they relate to. If meta.var columns correspond
to groups of variable (eg different sources for groups of variables),
two dataframes need to be supplied:

-   One containing the group meta.var data with column names indicating
    variable group names eg:

<table>
<caption>Example ref meta.var data where reference is same across groups of variables but varies across species</caption>
<thead>
<tr class="header">
<th align="left">species</th>
<th align="left">Diet</th>
<th align="left">ForStrat</th>
<th align="left">unsexed.mass</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Struthio_camelus</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Dunning08</td>
</tr>
<tr class="even">
<td align="left">Rhea_americana</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Dunning08</td>
</tr>
<tr class="odd">
<td align="left">Rhea_pennata</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Dunning08</td>
</tr>
<tr class="even">
<td align="left">Casuarius_casuarius</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Dunning08</td>
</tr>
<tr class="odd">
<td align="left">Casuarius_bennetti</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Handbook of the Birds of the World vol. 1. Del Hoyo J., Elliott A., Sargatal J. &amp; Christie D.A. (1992-2011). Lynx Edicions, Barcelona</td>
<td align="left">Dunning08</td>
</tr>
</tbody>
</table>

-   A separate two column dataframe, with columns named **`var`** and
    **`grp`** linking individual variables to group meta.var names in
    the first dataframe, eg:

<table>
<caption>Example ref meta.var data group to variable cross-reference table</caption>
<thead>
<tr class="header">
<th align="left"></th>
<th align="left">var</th>
<th align="left">grp</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">2</td>
<td align="left">Diet.Vend</td>
<td align="left">Diet</td>
</tr>
<tr class="even">
<td align="left">3</td>
<td align="left">Diet.Vect</td>
<td align="left">Diet</td>
</tr>
<tr class="odd">
<td align="left">12</td>
<td align="left">ForStrat.watbelowsurf</td>
<td align="left">ForStrat</td>
</tr>
<tr class="even">
<td align="left">13</td>
<td align="left">ForStrat.wataroundsurf</td>
<td align="left">ForStrat</td>
</tr>
<tr class="odd">
<td align="left">22</td>
<td align="left">unsexed.mass</td>
<td align="left">unsexed.mass</td>
</tr>
</tbody>
</table>

Note that variable names for most meta.vars can be assigned `NA` under
`grp` in the `_group` data.frame in which case NA will be assigned for
that meta.var for each variable observation. However, references MUST be
provided for all variables and matching will not proceed until this
condition has been met.

**`syn.links`**
---------------

syn.links needs to be a two column data.frame of unique synonym links

<br>

##### **metadata/**

-   `[metadata]`: should contain a **"metadata.csv"** file with
    information on all variables in the master datasheet.
-   `[vnames]`: table of variable name correspondence across datasets.

<br>

##### **taxo/**

-   `[taxo.table]`: table containing taxonomic information

<br>

**example workflow**
--------------------

In this example we will demonstrate the use of ***rmacroRDM functions***
to merge datasets **`D0`** and **`D1`** into a **`master`** datasheet.

We will use `D0` to set up the `master` and then merge dataset `D1` to
it.

First set the location of the **input/**, **output/** and **script/**
folders. Make sure the
[**`functions.R`**](https://github.com/annakrystalli/rmacroRDM/blob/master/R/functions.R)
and
[**`wideData_function.R`**](https://github.com/annakrystalli/rmacroRDM/blob/master/R/wideData_function.R)
scripts are saved in the **scripts/** folder and `source`.

    ### SETTINGS ##############################################################

    options(stringsAsFactors = F)

    output.folder <- "~/Documents/workflows/rmacroRDM/data/output/"
    input.folder <- "~/Documents/workflows/rmacroRDM/data/input/"
    script.folder <- "~/Documents/workflows/rmacroRDM/R/"

    # Functions & Packages

    require(dplyr)


    # source rmacroRDM functions
    source(paste(script.folder, "functions.R", sep = ""))
    source(paste(script.folder, "wideData_function.R", sep = ""))

<br>

Also we set a number of parameters which will configure the master and
spp.list setup.

    # master settings
    var.vars <- c("var", "value", "data.ID")
    match.vars <- c("synonyms", "data.status")
    meta.vars = c("qc", "observer", "ref", "n", "notes")
    master.vars <- c("species", match.vars, var.vars, meta.vars)

    # spp.list settings
    taxo.vars <- c("genus", "family", "order")

    # custom meta.variables can be assigned by supplying a vector of character strings to the metadata
    # argument in function setupInputFolder()


    setupInputFolder(input.folder, meta.vars)

Once folders are correctly populated, load D0.

    D0 <- read.csv(file = paste(input.folder, "csv/D0.csv", sep = "") ,fileEncoding = "mac")

<table>
<caption>D0</caption>
<thead>
<tr class="header">
<th align="left">species</th>
<th align="left">genus</th>
<th align="left">family</th>
<th align="left">order</th>
<th align="left">synonyms</th>
<th align="left">data.status</th>
<th align="left">var</th>
<th align="right">value</th>
<th align="left">data.ID</th>
<th align="left">qc</th>
<th align="left">observer</th>
<th align="left">ref</th>
<th align="left">n</th>
<th align="left">notes</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Acanthis_flammea</td>
<td align="left">Acanthis</td>
<td align="left">Fringillidae</td>
<td align="left">PASSERIFORMES</td>
<td align="left">Acanthis_flammea</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Healy, Kev ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Acanthis_hornemanni</td>
<td align="left">Acanthis</td>
<td align="left">Fringillidae</td>
<td align="left">PASSERIFORMES</td>
<td align="left">Acanthis_hornemanni</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Healy, Kev ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Acanthisitta_chloris</td>
<td align="left">Acanthisitta</td>
<td align="left">Acanthisittidae</td>
<td align="left">PASSERIFORMES</td>
<td align="left">Acanthisitta_chloris</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_badius</td>
<td align="left">Accipiter</td>
<td align="left">Accipitridae</td>
<td align="left">ACCIPITRIFORMES</td>
<td align="left">Accipiter_badius</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Accipiter_brevipes</td>
<td align="left">Accipiter</td>
<td align="left">Accipitridae</td>
<td align="left">ACCIPITRIFORMES</td>
<td align="left">Accipiter_brevipes</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_cooperii</td>
<td align="left">Accipiter</td>
<td align="left">Accipitridae</td>
<td align="left">ACCIPITRIFORMES</td>
<td align="left">Accipiter_cooperii</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Accipiter_gentilis</td>
<td align="left">Accipiter</td>
<td align="left">Accipitridae</td>
<td align="left">ACCIPITRIFORMES</td>
<td align="left">Accipiter_gentilis</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_melanoleucus</td>
<td align="left">Accipiter</td>
<td align="left">Accipitridae</td>
<td align="left">ACCIPITRIFORMES</td>
<td align="left">Accipiter_melanoleucus</td>
<td align="left">original</td>
<td align="left">activity.period</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">Siblya, Ri ...</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

#### create **`[spp.list]`** object

The next step in setting up our master datasheet is to assign the
species list to which all other data are to be matched. In our example,
we are using the **species list** in **dataset `D0`** to which we will
then add dataset **`D1.csv`**. We can also store taxonomic information
on the spp.list, by supplying a `[taxo.dat]` containing taxonomic
information on all species in `species` and `{taxo.vars}`

Columns `master.spp` and `rel.spp` keep track of any species added
during the matching process in order to retain data points, rather than
discard duplicate datapoints in the dataset to be merged that might be
matching to the same individual species on the master species list. In
the case of an added species, the value in `master.spp` will be
**FALSE** and `rel.spp` will contain the name of the single species in
master species list that the matching function identified duplicate
matches with. This allows all possible data to be retained but the
information in the spp.list allows such datapoints to be removed from
analyses if required.

If there is taxonomic data, this can be included in the **`spp.list`**
data.frame. For example, `D0` contains further taxonomic data on
**genus**, **family**, **order**. We add this to the **`spp.list`**
dataframe:

    # Create taxo.table
    taxo.dat <- unique(D0[,c("species", taxo.vars)])

    spp.list <- createSpp.list(species = taxo.dat$species, 
                               taxo.dat = taxo.dat, 
                               taxo.vars)

    head(spp.list)

    ##                species master.spp rel.spp taxo.status        genus
    ## 1     Acanthis_flammea       TRUE      NA    original     Acanthis
    ## 2  Acanthis_hornemanni       TRUE      NA    original     Acanthis
    ## 3 Acanthisitta_chloris       TRUE      NA    original Acanthisitta
    ## 4     Accipiter_badius       TRUE      NA    original    Accipiter
    ## 5   Accipiter_brevipes       TRUE      NA    original    Accipiter
    ## 6   Accipiter_cooperii       TRUE      NA    original    Accipiter
    ##            family           order
    ## 1    Fringillidae   PASSERIFORMES
    ## 2    Fringillidae   PASSERIFORMES
    ## 3 Acanthisittidae   PASSERIFORMES
    ## 4    Accipitridae ACCIPITRIFORMES
    ## 5    Accipitridae ACCIPITRIFORMES
    ## 6    Accipitridae ACCIPITRIFORMES

#### load **`[metadata]`**

<table>
<caption>T2: example variable metadata sheet</caption>
<thead>
<tr class="header">
<th align="left"></th>
<th align="left">code</th>
<th align="left">cat</th>
<th align="left">descr</th>
<th align="left">scores</th>
<th align="left">levels</th>
<th align="left">type</th>
<th align="left">units</th>
<th align="left">notes</th>
<th align="left">log</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">2</td>
<td align="left">bite</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Species responds to being touched on the beak with biting</td>
<td align="left">0;1</td>
<td align="left">FALSE;TRUE</td>
<td align="left">Bin</td>
<td align="left"></td>
<td align="left">If the bird bite when researchers put the index finger of the right hand against the beak. 0: not bite, 1: bite</td>
<td align="left">FALSE</td>
</tr>
<tr class="even">
<td align="left">4</td>
<td align="left">brood.par</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Percentage of cuckoo parasitized nests of a given host species</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left">%</td>
<td align="left"></td>
<td align="left">FALSE</td>
</tr>
<tr class="odd">
<td align="left">5</td>
<td align="left">coloniality</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Coloniality</td>
<td align="left">0;1;2</td>
<td align="left">Solitary;Colonial;Semicolonial</td>
<td align="left">Cat</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">FALSE</td>
</tr>
<tr class="even">
<td align="left">6</td>
<td align="left">colony.size</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Maximum colony size</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Int</td>
<td align="left"></td>
<td align="left">transformed toa a log -10 scale; 0: Solitary to 5: Colony size exceeding 100.000</td>
<td align="left">FALSE</td>
</tr>
<tr class="odd">
<td align="left">7</td>
<td align="left">disperse.dist</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Maximum dispersal distance</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left">km</td>
<td align="left">the minimum distance from the mainland to an island with a permanent breeding population</td>
<td align="left">TRUE</td>
</tr>
<tr class="even">
<td align="left">8</td>
<td align="left">diving</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Species dives</td>
<td align="left">0;1</td>
<td align="left">FALSE;TRUE</td>
<td align="left">Bin</td>
<td align="left"></td>
<td align="left">1:Diving, 0:No diving</td>
<td align="left">FALSE</td>
</tr>
<tr class="odd">
<td align="left">9</td>
<td align="left">egg.reject</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Rejection rate of parasitic eggs laid in the host nests</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left">%</td>
<td align="left"></td>
<td align="left">FALSE</td>
</tr>
<tr class="even">
<td align="left">10</td>
<td align="left">fear.scream</td>
<td align="left">BEHAVIORAL</td>
<td align="left">Fear scream score during handling (0-1)</td>
<td align="left"></td>
<td align="left"></td>
<td align="left">Con</td>
<td align="left"></td>
<td align="left">While the bird was handled, if it have a fear scream score of 1, if not score of 0</td>
<td align="left">FALSE</td>
</tr>
</tbody>
</table>

#### load **`syn.links`**

    ##               synonyms             species
    ## 1 Pteroicnemia_pennata Pterocnemia_pennata
    ## 2   Casuarius_casurius Casuarius_casuarius
    ## 3       Apteryx_haasti     Apteryx_haastii
    ## 4        Apteryx_oweni      Apteryx_owenii
    ## 5      Nothura_darwini    Nothura_darwinii
    ## 6      Toaniscus_nanus     Taoniscus_nanus

<br>

#### create master object

Finally create the **`[[master]]`** object.

    # create master shell
    master <- list(data = newMasterData(master.vars), spp.list = spp.list, metadata = metadata)

`D0` is almost in the master data format, we just need to remove all
taxonomic information. In this case, I subset `D0` to variables not in
`{taxo.vars}`. Now that `D0` is in the master data format, I can update
the empty `[[master]]` object with the data. Also, because the species
list was generated from D0, we do not need to update the `spp.list`,
although the function checks for species matching anyways.

    D0 <- D0[,!names(D0) %in% taxo.vars]

    master <- updateMaster(master, data = D0, spp.list = NULL)

    str(master)

    ## List of 3
    ##  $ data    :'data.frame':    53605 obs. of  11 variables:
    ##   ..$ species    : chr [1:53605] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ synonyms   : chr [1:53605] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ data.status: chr [1:53605] "original" "original" "original" "original" ...
    ##   ..$ var        : chr [1:53605] "activity.period" "activity.period" "activity.period" "activity.period" ...
    ##   ..$ value      : num [1:53605] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ data.ID    : logi [1:53605] NA NA NA NA NA NA ...
    ##   ..$ qc         : logi [1:53605] NA NA NA NA NA NA ...
    ##   ..$ observer   : logi [1:53605] NA NA NA NA NA NA ...
    ##   ..$ ref        : chr [1:53605] "Healy, Kev ..." "Healy, Kev ..." "Siblya, Ri ..." "Siblya, Ri ..." ...
    ##   ..$ n          : logi [1:53605] NA NA NA NA NA NA ...
    ##   ..$ notes      : logi [1:53605] NA NA NA NA NA NA ...
    ##  $ spp.list:'data.frame':    4384 obs. of  7 variables:
    ##   ..$ species    : chr [1:4384] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ master.spp : logi [1:4384] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ rel.spp    : logi [1:4384] NA NA NA NA NA NA ...
    ##   ..$ taxo.status: chr [1:4384] "original" "original" "original" "original" ...
    ##   ..$ genus      : chr [1:4384] "Acanthis" "Acanthis" "Acanthisitta" "Accipiter" ...
    ##   ..$ family     : chr [1:4384] "Fringillidae" "Fringillidae" "Acanthisittidae" "Accipitridae" ...
    ##   ..$ order      : chr [1:4384] "PASSERIFORMES" "PASSERIFORMES" "PASSERIFORMES" "ACCIPITRIFORMES" ...
    ##  $ metadata:'data.frame':    208 obs. of  14 variables:
    ##   ..$ code      : chr [1:208] "aerial.for" "bite" "breed.system" "brood.par" ...
    ##   ..$ orig.vname: chr [1:208] "Aerial foraging" "Biting" "Breeding system" "Brood parasitism" ...
    ##   ..$ cat       : chr [1:208] "BEHAVIORAL" "BEHAVIORAL" "BEHAVIORAL" "BEHAVIORAL" ...
    ##   ..$ n.total.  : chr [1:208] "130" "89" "965" "85" ...
    ##   ..$ n.100.    : chr [1:208] "100" "64" "97" "68" ...
    ##   ..$ descr     : chr [1:208] "Species relies on aerial foraging?" "Species responds to being touched on the beak with biting" "Which adult(s) provides the majority of care:" "Percentage of cuckoo parasitized nests of a given host species" ...
    ##   ..$ scores    : chr [1:208] "0;1" "0;1" "1;2;3;4;5" "" ...
    ##   ..$ levels    : chr [1:208] "FALSE;TRUE" "FALSE;TRUE" "Pair;Female;Male;Cooperative;Occassional" "" ...
    ##   ..$ type      : chr [1:208] "Bin" "Bin" "Cat" "Con" ...
    ##   ..$ units     : chr [1:208] "" "" "" "%" ...
    ##   ..$ ref.total : chr [1:208] "24, [80], [88], 112" "43, 76" "24, 112, (125)" "(4), 36" ...
    ##   ..$ notes     : chr [1:208] "Does species rely on flight to obtain food?  aerial foragers: 1, and if not are not aerial foragers: 0. In the case of ref. 88 "| __truncated__ "If the bird bite when researchers put the index finger of the right hand against the beak.  0: not bite, 1: bite" "1: Pair, 2: Female, 3:Male, 4: Cooperative (regularly breeds cooperatively, with non-parents providing parental care), 5: Ocass"| __truncated__ "" ...
    ##   ..$ source    : chr [1:208] "original" "original" "original" "original" ...
    ##   ..$ log       : chr [1:208] "FALSE" "FALSE" "FALSE" "FALSE" ...

#### create `[[m]]` object

Next assign the dataset filename, to be used to automate data loading

    filename <- "D1"
      
      m <- matchObj(data.ID = "D1", spp.list = spp.list, status = "unmatched",
                    data = read.csv(paste(input.folder, "csv/", filename, ".csv", sep = ""),
                                    stringsAsFactors=FALSE),
                    sub = "spp.list", filename = filename, 
                    meta = createMeta(meta.vars)) # use addMeta function to manually add metadata.

Here, we use the filename to load the data into `matchObj()`. We define
`"spp.list"` as the sub dataset. The match functions will therefore
identify and attempt to match unmatched species names in the
`{spp.list$species}`. We also create a `[[meta]]` object using function
`createMeta(meta.vars)` and supplying `{meta.vars}`.

The resulting **`[[m]]`** object has the following structure:

    str(m)

    ## List of 8
    ##  $ data.ID : chr "D1"
    ##  $ spp.list:'data.frame':    4384 obs. of  7 variables:
    ##   ..$ species    : chr [1:4384] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ master.spp : logi [1:4384] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ rel.spp    : logi [1:4384] NA NA NA NA NA NA ...
    ##   ..$ taxo.status: chr [1:4384] "original" "original" "original" "original" ...
    ##   ..$ genus      : chr [1:4384] "Acanthis" "Acanthis" "Acanthisitta" "Accipiter" ...
    ##   ..$ family     : chr [1:4384] "Fringillidae" "Fringillidae" "Acanthisittidae" "Accipitridae" ...
    ##   ..$ order      : chr [1:4384] "PASSERIFORMES" "PASSERIFORMES" "PASSERIFORMES" "ACCIPITRIFORMES" ...
    ##  $ data    :'data.frame':    9802 obs. of  20 variables:
    ##   ..$ species         : chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   ..$ repro.age       : num [1:9802] NA 363 NA NA 363 ...
    ##   ..$ clutch.size     : num [1:9802] NA 3.25 2.7 NA 4 NA 2.7 4.25 3.25 4.35 ...
    ##   ..$ clutch.no       : num [1:9802] NA 1 NA NA 1 NA NA 1 NA 1 ...
    ##   ..$ body.mass       : num [1:9802] 252 140 345 142 204 ...
    ##   ..$ max.long        : num [1:9802] NA NA NA NA NA ...
    ##   ..$ body.mass.hatch : num [1:9802] NA NA NA NA NA NA NA NA NA 28 ...
    ##   ..$ egg.mass        : num [1:9802] NA 21 32 NA 21.9 ...
    ##   ..$ inc             : num [1:9802] NA 30 NA NA 32.5 ...
    ##   ..$ fledge.age      : num [1:9802] NA 32 NA NA 42.5 ...
    ##   ..$ long            : num [1:9802] NA NA NA NA NA ...
    ##   ..$ interclutch     : num [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ body.mass.f     : num [1:9802] 352 168 390 NA 230 ...
    ##   ..$ body.mass.m     : num [1:9802] 223 125 212 142 170 ...
    ##   ..$ body.mass.u     : num [1:9802] NA 123 NA NA NA ...
    ##   ..$ egg.width       : num [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ egg.length      : num [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ body.mass.fledge: num [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ repro.age.diff  : num [1:9802] NA NA NA NA NA ...
    ##   ..$ qc              : int [1:9802] 3 3 3 1 2 1 1 1 2 1 ...
    ##  $ sub     : chr "spp.list"
    ##  $ set     : chr "data"
    ##  $ status  : chr "unmatched"
    ##  $ meta    :List of 5
    ##   ..$ qc      : NULL
    ##   ..$ observer: NULL
    ##   ..$ ref     : NULL
    ##   ..$ n       : NULL
    ##   ..$ notes   : NULL
    ##  $ filename: chr "D1"

<br>

#### process `[[m]]` object

Once the `[[m]]` object is created, I pipe it a number of through the
**`rmacroRDM`** processing functions:

      m <- processDat(m, input.folder, var.omit) %>% 
        separateDatMeta() %>% 
        compileMeta(input.folder = input.folder) %>%
        checkVarMeta(master$metadata) %>%
        dataMatchPrep()

<br>

#### let's take a closer look

**`processDat()`** cleans the data and removes unwanted variables.

      m <- processDat(m, input.folder, var.omit = NULL)

    ## Loading required package: stringr

      str(m$data)

    ## 'data.frame':    9802 obs. of  20 variables:
    ##  $ species         : chr  "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##  $ repro.age       : num  NA 363 NA NA 363 ...
    ##  $ clutch.size     : num  NA 3.25 2.7 NA 4 NA 2.7 4.25 3.25 4.35 ...
    ##  $ clutch.no       : num  NA 1 NA NA 1 NA NA 1 NA 1 ...
    ##  $ body.mass       : num  252 140 345 142 204 ...
    ##  $ max.long        : num  NA NA NA NA NA ...
    ##  $ body.mass.hatch : num  NA NA NA NA NA NA NA NA NA 28 ...
    ##  $ egg.mass        : num  NA 21 32 NA 21.9 ...
    ##  $ inc             : num  NA 30 NA NA 32.5 ...
    ##  $ fledge.age      : num  NA 32 NA NA 42.5 ...
    ##  $ long            : num  NA NA NA NA NA ...
    ##  $ interclutch     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.f     : num  352 168 390 NA 230 ...
    ##  $ body.mass.m     : num  223 125 212 142 170 ...
    ##  $ body.mass.u     : num  NA 123 NA NA NA ...
    ##  $ egg.width       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ egg.length      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.fledge: num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ repro.age.diff  : num  NA NA NA NA NA ...
    ##  $ qc              : int  3 3 3 1 2 1 1 1 2 1 ...

<br>

**`separateDatMeta()`** separates data columns from data, correctly
appended as `{meta.vars}`. In this case, the data column `qc` is
separated and processed into a valid `meta.var` element and appended to
`[[meta]]$qc`.

      m <- separateDatMeta(m)
      
      str(m$data)

    ## 'data.frame':    9802 obs. of  19 variables:
    ##  $ species         : chr  "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##  $ repro.age       : num  NA 363 NA NA 363 ...
    ##  $ clutch.size     : num  NA 3.25 2.7 NA 4 NA 2.7 4.25 3.25 4.35 ...
    ##  $ clutch.no       : num  NA 1 NA NA 1 NA NA 1 NA 1 ...
    ##  $ body.mass       : num  252 140 345 142 204 ...
    ##  $ max.long        : num  NA NA NA NA NA ...
    ##  $ body.mass.hatch : num  NA NA NA NA NA NA NA NA NA 28 ...
    ##  $ egg.mass        : num  NA 21 32 NA 21.9 ...
    ##  $ inc             : num  NA 30 NA NA 32.5 ...
    ##  $ fledge.age      : num  NA 32 NA NA 42.5 ...
    ##  $ long            : num  NA NA NA NA NA ...
    ##  $ interclutch     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.f     : num  352 168 390 NA 230 ...
    ##  $ body.mass.m     : num  223 125 212 142 170 ...
    ##  $ body.mass.u     : num  NA 123 NA NA NA ...
    ##  $ egg.width       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ egg.length      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.fledge: num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ repro.age.diff  : num  NA NA NA NA NA ...

      str(m$meta)

    ## List of 5
    ##  $ qc      :'data.frame':    9802 obs. of  2 variables:
    ##   ..$ species: chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   ..$ all    : int [1:9802] 3 3 3 1 2 1 1 1 2 1 ...
    ##  $ observer: NULL
    ##  $ ref     : NULL
    ##  $ n       : NULL
    ##  $ notes   : NULL

<br>

**`compileMeta()`** automates the process of sourcing, checking and
setting up metadata to be compiled into the long data format. It will
check through the input.folder `{file.sytem}` for correctly labelled and
filed `{meta.vars}` data and compile it into missing `m$[[meta]]`
elements.

In this case it appends data automatically loaded from the `meta.var`
folders. Only data in **ref/** and **n/** have been supplied. Data in
**ref/** contains reference data for all species and variables in a
single `.csv` file: **`D1.csv`**.

    ## 'data.frame':    9802 obs. of  28 variables:
    ##  $ species                       : chr  "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##  $ repro.age                     : chr  NA "Bennett, 1986 ..." NA NA ...
    ##  $ clutch.size                   : chr  NA "mean of BC Bird ..." "mean of Pereira ..." NA ...
    ##  $ clutch.no                     : chr  NA "Bennett, 1986 ..." NA NA ...
    ##  $ body.mass                     : chr  "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Bennett, 1986 f ..." "Dunning, 1992 ..." ...
    ##  $ max.long                      : chr  NA NA NA NA ...
    ##  $ gestation_d                   : chr  NA NA NA NA ...
    ##  $ nest.period                   : chr  NA NA NA NA ...
    ##  $ body.mass.hatch               : chr  NA NA NA NA ...
    ##  $ weaning_weight_g              : chr  NA NA NA NA ...
    ##  $ egg.mass                      : chr  NA "Szekely, Lislev ..." "Bennett, 1986 ..." NA ...
    ##  $ inc                           : chr  NA "BC Birds - Wils ..." NA NA ...
    ##  $ fledge.age                    : chr  NA "Bennett, 1986 f ..." NA NA ...
    ##  $ long                          : chr  NA NA NA NA ...
    ##  $ interclutch                   : chr  NA NA NA NA ...
    ##  $ body.mass.f                   : chr  "mean of Dunning ..." "mean of Dunning ..." "Dunning, 1992 f ..." NA ...
    ##  $ body.mass.m                   : chr  "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Dunning, 1992 ..." ...
    ##  $ body.mass.u                   : chr  NA "Szekely, Lislev ..." NA NA ...
    ##  $ egg.width                     : chr  NA NA NA NA ...
    ##  $ egg.length                    : chr  NA NA NA NA ...
    ##  $ body.mass.fledge              : chr  NA NA NA NA ...
    ##  $ bill.len.sm                   : chr  NA NA NA NA ...
    ##  $ bill.len.sf                   : chr  NA NA NA NA ...
    ##  $ birth_or_hatching_svl_cm      : chr  NA NA NA NA ...
    ##  $ female_svl_at_maturity_cm     : chr  NA NA NA NA ...
    ##  $ female_body_mass_at_maturity_g: chr  NA NA NA NA ...
    ##  $ no_sex_svl_cm                 : chr  NA NA NA NA ...
    ##  $ repro.age.diff                : chr  NA NA NA NA ...

Data in **n/** are given again in **`D1.csv`**:

    ## 'data.frame':    9802 obs. of  26 variables:
    ##  $ species                     : chr  "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##  $ repro.age                   : int  NA 1 NA NA 1 NA NA 3 NA 3 ...
    ##  $ clutch.size                 : int  NA 6 4 NA 3 NA 4 6 4 6 ...
    ##  $ clutch.no                   : int  NA 1 NA NA 1 NA NA 3 NA 3 ...
    ##  $ body.mass                   : int  4 7 5 1 6 4 7 9 6 9 ...
    ##  $ gestation                   : logi  NA NA NA NA NA NA ...
    ##  $ weaning                     : logi  NA NA NA NA NA NA ...
    ##  $ body.mass.hatch             : int  NA NA NA NA NA NA NA NA NA 1 ...
    ##  $ weaning_weight              : logi  NA NA NA NA NA NA ...
    ##  $ egg.mass                    : int  NA 3 1 NA 2 NA 1 3 2 3 ...
    ##  $ inc                         : int  NA 5 NA NA 1 NA NA 2 NA 2 ...
    ##  $ fledge.age                  : int  NA 3 NA NA 1 NA NA 2 NA 2 ...
    ##  $ interclutch                 : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.f                 : int  2 4 3 NA 4 3 5 5 4 5 ...
    ##  $ body.mass.m                 : int  3 3 3 1 3 3 3 6 4 5 ...
    ##  $ body.mass.u                 : int  NA 1 NA NA NA NA NA 1 NA 1 ...
    ##  $ egg.width                   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ egg.length                  : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.fledge            : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ male_svl                    : logi  NA NA NA NA NA NA ...
    ##  $ female_svl                  : logi  NA NA NA NA NA NA ...
    ##  $ hatching_svl                : logi  NA NA NA NA NA NA ...
    ##  $ female_svl_at_maturity      : logi  NA NA NA NA NA NA ...
    ##  $ female_body_mass_at_maturity: logi  NA NA NA NA NA NA ...
    ##  $ no_sex_svl                  : logi  NA NA NA NA NA NA ...
    ##  $ repro.age.diff              : int  NA 1 NA NA 1 NA NA 4 NA 4 ...

However `n` data are missing for some `vars` so a group cross-reference
table **`D1_n_group.csv`** is also supplied. This table was used to
check variable matches and confirm missing meta.var data as NA. Note.
NAs not allowed for `meta.var == "ref"`.

    ## 'data.frame':    18 obs. of  2 variables:
    ##  $ var: chr  "repro.age" "clutch.size" "clutch.no" "body.mass" ...
    ##  $ grp: chr  "repro.age" "clutch.size" "clutch.no" "body.mass" ...

      m <- compileMeta(m, input.folder = input.folder)

    ## [1] "Warning: NULL data for meta.var: observer"
    ## [1] "n vars matched successfully to _meta.var_group"
    ## [1] "Warning: NULL data for meta.var: notes"

      str(m$meta)

    ## List of 5
    ##  $ qc      :'data.frame':    9802 obs. of  2 variables:
    ##   ..$ species: chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   ..$ all    : int [1:9802] 3 3 3 1 2 1 1 1 2 1 ...
    ##  $ observer: NULL
    ##  $ ref     :'data.frame':    9802 obs. of  19 variables:
    ##   ..$ species         : chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   ..$ repro.age       : chr [1:9802] NA "Bennett, 1986 ..." NA NA ...
    ##   ..$ clutch.size     : chr [1:9802] NA "mean of BC Bird ..." "mean of Pereira ..." NA ...
    ##   ..$ clutch.no       : chr [1:9802] NA "Bennett, 1986 ..." NA NA ...
    ##   ..$ body.mass       : chr [1:9802] "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Bennett, 1986 f ..." "Dunning, 1992 ..." ...
    ##   ..$ max.long        : chr [1:9802] NA NA NA NA ...
    ##   ..$ body.mass.hatch : chr [1:9802] NA NA NA NA ...
    ##   ..$ egg.mass        : chr [1:9802] NA "Szekely, Lislev ..." "Bennett, 1986 ..." NA ...
    ##   ..$ inc             : chr [1:9802] NA "BC Birds - Wils ..." NA NA ...
    ##   ..$ fledge.age      : chr [1:9802] NA "Bennett, 1986 f ..." NA NA ...
    ##   ..$ long            : chr [1:9802] NA NA NA NA ...
    ##   ..$ interclutch     : chr [1:9802] NA NA NA NA ...
    ##   ..$ body.mass.f     : chr [1:9802] "mean of Dunning ..." "mean of Dunning ..." "Dunning, 1992 f ..." NA ...
    ##   ..$ body.mass.m     : chr [1:9802] "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Dunning, 1992 ..." ...
    ##   ..$ body.mass.u     : chr [1:9802] NA "Szekely, Lislev ..." NA NA ...
    ##   ..$ egg.width       : chr [1:9802] NA NA NA NA ...
    ##   ..$ egg.length      : chr [1:9802] NA NA NA NA ...
    ##   ..$ body.mass.fledge: chr [1:9802] NA NA NA NA ...
    ##   ..$ repro.age.diff  : chr [1:9802] NA NA NA NA ...
    ##  $ n       :'data.frame':    9802 obs. of  17 variables:
    ##   ..$ species         : chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   ..$ repro.age       : int [1:9802] NA 1 NA NA 1 NA NA 3 NA 3 ...
    ##   ..$ clutch.size     : int [1:9802] NA 6 4 NA 3 NA 4 6 4 6 ...
    ##   ..$ clutch.no       : int [1:9802] NA 1 NA NA 1 NA NA 3 NA 3 ...
    ##   ..$ body.mass       : int [1:9802] 4 7 5 1 6 4 7 9 6 9 ...
    ##   ..$ body.mass.hatch : int [1:9802] NA NA NA NA NA NA NA NA NA 1 ...
    ##   ..$ egg.mass        : int [1:9802] NA 3 1 NA 2 NA 1 3 2 3 ...
    ##   ..$ inc             : int [1:9802] NA 5 NA NA 1 NA NA 2 NA 2 ...
    ##   ..$ fledge.age      : int [1:9802] NA 3 NA NA 1 NA NA 2 NA 2 ...
    ##   ..$ interclutch     : int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ body.mass.f     : int [1:9802] 2 4 3 NA 4 3 5 5 4 5 ...
    ##   ..$ body.mass.m     : int [1:9802] 3 3 3 1 3 3 3 6 4 5 ...
    ##   ..$ body.mass.u     : int [1:9802] NA 1 NA NA NA NA NA 1 NA 1 ...
    ##   ..$ egg.width       : int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ egg.length      : int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ body.mass.fledge: int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ repro.age.diff  : int [1:9802] NA 1 NA NA 1 NA NA 4 NA 4 ...
    ##  $ notes   : NULL

As can be seen, data for `{meta.vars}` `"ref"` and `"n"` have been
processed and appended to the appropriate `m$[[meta]]` element.

<br>

**`checkVarMeta()`** checks that all `vars` in `m$[data]` have valid
metadata information in `[metadata]`

      m <- checkVarMeta(m, master$metadata)

    ## [1] "D1 metadata complete"

All good.

<br>

**`dataMatchPrep()`** prepares `m$[data]` to track synonym matching.

      m <- dataMatchPrep(m)
      
      str(m$data)

    ## 'data.frame':    9802 obs. of  21 variables:
    ##  $ species         : chr  "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##  $ synonyms        : chr  "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##  $ data.status     : chr  "original" "original" "original" "original" ...
    ##  $ repro.age       : num  NA 363 NA NA 363 ...
    ##  $ clutch.size     : num  NA 3.25 2.7 NA 4 NA 2.7 4.25 3.25 4.35 ...
    ##  $ clutch.no       : num  NA 1 NA NA 1 NA NA 1 NA 1 ...
    ##  $ body.mass       : num  252 140 345 142 204 ...
    ##  $ max.long        : num  NA NA NA NA NA ...
    ##  $ body.mass.hatch : num  NA NA NA NA NA NA NA NA NA 28 ...
    ##  $ egg.mass        : num  NA 21 32 NA 21.9 ...
    ##  $ inc             : num  NA 30 NA NA 32.5 ...
    ##  $ fledge.age      : num  NA 32 NA NA 42.5 ...
    ##  $ long            : num  NA NA NA NA NA ...
    ##  $ interclutch     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.f     : num  352 168 390 NA 230 ...
    ##  $ body.mass.m     : num  223 125 212 142 170 ...
    ##  $ body.mass.u     : num  NA 123 NA NA NA ...
    ##  $ egg.width       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ egg.length      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ body.mass.fledge: num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ repro.age.diff  : num  NA NA NA NA NA ...

<br>

------------------------------------------------------------------------

#### match `[[m]]` object

     m <- dataSppMatch(m, ignore.unmatched = T, 
                        syn.links = syn.links, addSpp = T)

    ## [1] "match incomplete, 58 spp.list datapoints unmatched"

     str(m)

    ## List of 9
    ##  $ data.ID  : chr "D1"
    ##  $ spp.list :'data.frame':   4384 obs. of  7 variables:
    ##   ..$ species    : chr [1:4384] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ master.spp : logi [1:4384] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ rel.spp    : logi [1:4384] NA NA NA NA NA NA ...
    ##   ..$ taxo.status: chr [1:4384] "original" "original" "original" "original" ...
    ##   ..$ genus      : chr [1:4384] "Acanthis" "Acanthis" "Acanthisitta" "Accipiter" ...
    ##   ..$ family     : chr [1:4384] "Fringillidae" "Fringillidae" "Acanthisittidae" "Accipitridae" ...
    ##   ..$ order      : chr [1:4384] "PASSERIFORMES" "PASSERIFORMES" "PASSERIFORMES" "ACCIPITRIFORMES" ...
    ##  $ data     :'data.frame':   4326 obs. of  21 variables:
    ##   ..$ species         : chr [1:4326] "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brevipes" "Accipiter_cirrocephalus" ...
    ##   ..$ synonyms        : chr [1:4326] "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brevipes" "Accipiter_cirrocephalus" ...
    ##   ..$ data.status     : chr [1:4326] "original" "original" "original" "original" ...
    ##   ..$ repro.age       : num [1:4326] 363 NA 363 NA 730 ...
    ##   ..$ clutch.size     : num [1:4326] 3.25 2.7 4 3.25 4.35 ...
    ##   ..$ clutch.no       : num [1:4326] 1 NA 1 NA 1 NA 1 1 1 NA ...
    ##   ..$ body.mass       : num [1:4326] 140 345 204 180 452 ...
    ##   ..$ max.long        : num [1:4326] NA NA NA NA 20.3 ...
    ##   ..$ body.mass.hatch : num [1:4326] NA NA NA NA 28 NA 37 NA 13.8 NA ...
    ##   ..$ egg.mass        : num [1:4326] 21 32 21.9 20.8 40 ...
    ##   ..$ inc             : num [1:4326] 30 NA 32.5 NA 29.8 ...
    ##   ..$ fledge.age      : num [1:4326] 32 NA 42.5 NA 32 ...
    ##   ..$ long            : num [1:4326] NA NA NA NA 12 ...
    ##   ..$ interclutch     : num [1:4326] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ body.mass.f     : num [1:4326] 168 390 230 231 529 ...
    ##   ..$ body.mass.m     : num [1:4326] 125 212 170 127 338 ...
    ##   ..$ body.mass.u     : num [1:4326] 123 NA NA NA 452 ...
    ##   ..$ egg.width       : num [1:4326] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ egg.length      : num [1:4326] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ body.mass.fledge: num [1:4326] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ repro.age.diff  : num [1:4326] NA NA NA NA 0 ...
    ##  $ sub      : chr "spp.list"
    ##  $ set      : chr "data"
    ##  $ status   : chr "incomplete_match: 58"
    ##  $ meta     :List of 5
    ##   ..$ qc      :'data.frame': 9802 obs. of  2 variables:
    ##   .. ..$ species: chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   .. ..$ all    : int [1:9802] 3 3 3 1 2 1 1 1 2 1 ...
    ##   ..$ observer: NULL
    ##   ..$ ref     :'data.frame': 9802 obs. of  19 variables:
    ##   .. ..$ species         : chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   .. ..$ repro.age       : chr [1:9802] NA "Bennett, 1986 ..." NA NA ...
    ##   .. ..$ clutch.size     : chr [1:9802] NA "mean of BC Bird ..." "mean of Pereira ..." NA ...
    ##   .. ..$ clutch.no       : chr [1:9802] NA "Bennett, 1986 ..." NA NA ...
    ##   .. ..$ body.mass       : chr [1:9802] "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Bennett, 1986 f ..." "Dunning, 1992 ..." ...
    ##   .. ..$ max.long        : chr [1:9802] NA NA NA NA ...
    ##   .. ..$ body.mass.hatch : chr [1:9802] NA NA NA NA ...
    ##   .. ..$ egg.mass        : chr [1:9802] NA "Szekely, Lislev ..." "Bennett, 1986 ..." NA ...
    ##   .. ..$ inc             : chr [1:9802] NA "BC Birds - Wils ..." NA NA ...
    ##   .. ..$ fledge.age      : chr [1:9802] NA "Bennett, 1986 f ..." NA NA ...
    ##   .. ..$ long            : chr [1:9802] NA NA NA NA ...
    ##   .. ..$ interclutch     : chr [1:9802] NA NA NA NA ...
    ##   .. ..$ body.mass.f     : chr [1:9802] "mean of Dunning ..." "mean of Dunning ..." "Dunning, 1992 f ..." NA ...
    ##   .. ..$ body.mass.m     : chr [1:9802] "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Dunning, 1992 f ..." "Dunning, 1992 ..." ...
    ##   .. ..$ body.mass.u     : chr [1:9802] NA "Szekely, Lislev ..." NA NA ...
    ##   .. ..$ egg.width       : chr [1:9802] NA NA NA NA ...
    ##   .. ..$ egg.length      : chr [1:9802] NA NA NA NA ...
    ##   .. ..$ body.mass.fledge: chr [1:9802] NA NA NA NA ...
    ##   .. ..$ repro.age.diff  : chr [1:9802] NA NA NA NA ...
    ##   ..$ n       :'data.frame': 9802 obs. of  17 variables:
    ##   .. ..$ species         : chr [1:9802] "Accipiter_albogularis" "Accipiter_badius" "Accipiter_bicolor" "Accipiter_brachyurus" ...
    ##   .. ..$ repro.age       : int [1:9802] NA 1 NA NA 1 NA NA 3 NA 3 ...
    ##   .. ..$ clutch.size     : int [1:9802] NA 6 4 NA 3 NA 4 6 4 6 ...
    ##   .. ..$ clutch.no       : int [1:9802] NA 1 NA NA 1 NA NA 3 NA 3 ...
    ##   .. ..$ body.mass       : int [1:9802] 4 7 5 1 6 4 7 9 6 9 ...
    ##   .. ..$ body.mass.hatch : int [1:9802] NA NA NA NA NA NA NA NA NA 1 ...
    ##   .. ..$ egg.mass        : int [1:9802] NA 3 1 NA 2 NA 1 3 2 3 ...
    ##   .. ..$ inc             : int [1:9802] NA 5 NA NA 1 NA NA 2 NA 2 ...
    ##   .. ..$ fledge.age      : int [1:9802] NA 3 NA NA 1 NA NA 2 NA 2 ...
    ##   .. ..$ interclutch     : int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ body.mass.f     : int [1:9802] 2 4 3 NA 4 3 5 5 4 5 ...
    ##   .. ..$ body.mass.m     : int [1:9802] 3 3 3 1 3 3 3 6 4 5 ...
    ##   .. ..$ body.mass.u     : int [1:9802] NA 1 NA NA NA NA NA 1 NA 1 ...
    ##   .. ..$ egg.width       : int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ egg.length      : int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ body.mass.fledge: int [1:9802] NA NA NA NA NA NA NA NA NA NA ...
    ##   .. ..$ repro.age.diff  : int [1:9802] NA 1 NA NA 1 NA NA 4 NA 4 ...
    ##   ..$ notes   : NULL
    ##  $ filename : chr "D1"
    ##  $ unmatched:'data.frame':   58 obs. of  2 variables:
    ##   ..$ species : chr [1:58] "Buphagus_erythrorynchus" "Cyanomitra_violacea" "Megascops_flammeolus" "Thalasseus_caspia" ...
    ##   ..$ synonyms: logi [1:58] NA NA NA NA NA NA ...

When `ignore.unmatched = T`,**sub** species that have not automatically
been matched to **set** species are ignored and omitted from the
dataset. When `ignore.unmatched = F`, the function halts and appends
`{unmatched}` species list to `[[m]]`.

#### compile data to master format

    output <- masterDataFormat(m, meta.vars, match.vars, var.vars)

    kable(head(output$data))

<table>
<thead>
<tr class="header">
<th align="left">species</th>
<th align="left">synonyms</th>
<th align="left">data.status</th>
<th align="left">var</th>
<th align="right">value</th>
<th align="left">data.ID</th>
<th align="right">qc</th>
<th align="left">observer</th>
<th align="left">ref</th>
<th align="left">n</th>
<th align="left">notes</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Accipiter_badius</td>
<td align="left">Accipiter_badius</td>
<td align="left">original</td>
<td align="left">repro.age</td>
<td align="right">363.468</td>
<td align="left">D1</td>
<td align="right">3</td>
<td align="left">NA</td>
<td align="left">Bennett, 1986 ...</td>
<td align="left">1</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_brevipes</td>
<td align="left">Accipiter_brevipes</td>
<td align="left">original</td>
<td align="left">repro.age</td>
<td align="right">363.468</td>
<td align="left">D1</td>
<td align="right">2</td>
<td align="left">NA</td>
<td align="left">Bennett, 1986 ...</td>
<td align="left">1</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Accipiter_cooperii</td>
<td align="left">Accipiter_cooperii</td>
<td align="left">original</td>
<td align="left">repro.age</td>
<td align="right">730.000</td>
<td align="left">D1</td>
<td align="right">1</td>
<td align="left">NA</td>
<td align="left">de Magalhaes an ...</td>
<td align="left">3</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_gentilis</td>
<td align="left">Accipiter_gentilis</td>
<td align="left">original</td>
<td align="left">repro.age</td>
<td align="right">636.835</td>
<td align="left">D1</td>
<td align="right">2</td>
<td align="left">NA</td>
<td align="left">mean of de Maga ...</td>
<td align="left">2</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Accipiter_melanoleucus</td>
<td align="left">Accipiter_melanoleucus</td>
<td align="left">original</td>
<td align="left">repro.age</td>
<td align="right">1090.404</td>
<td align="left">D1</td>
<td align="right">2</td>
<td align="left">NA</td>
<td align="left">Bennett, 1986 ...</td>
<td align="left">1</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Accipiter_nisus</td>
<td align="left">Accipiter_nisus</td>
<td align="left">original</td>
<td align="left">repro.age</td>
<td align="right">365.000</td>
<td align="left">D1</td>
<td align="right">3</td>
<td align="left">NA</td>
<td align="left">de Magalhaes an ...</td>
<td align="left">3</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

    master <- updateMaster(master, data = output$data, spp.list = output$spp.list)
     
    str(master)

    ## List of 3
    ##  $ data    :'data.frame':    84316 obs. of  11 variables:
    ##   ..$ species    : chr [1:84316] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ synonyms   : chr [1:84316] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ data.status: chr [1:84316] "original" "original" "original" "original" ...
    ##   ..$ var        : chr [1:84316] "activity.period" "activity.period" "activity.period" "activity.period" ...
    ##   ..$ value      : num [1:84316] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..$ data.ID    : chr [1:84316] NA NA NA NA ...
    ##   ..$ qc         : int [1:84316] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ observer   : logi [1:84316] NA NA NA NA NA NA ...
    ##   ..$ ref        : chr [1:84316] "Healy, Kev ..." "Healy, Kev ..." "Siblya, Ri ..." "Siblya, Ri ..." ...
    ##   ..$ n          : chr [1:84316] NA NA NA NA ...
    ##   ..$ notes      : logi [1:84316] NA NA NA NA NA NA ...
    ##  $ spp.list:'data.frame':    4384 obs. of  7 variables:
    ##   ..$ species    : chr [1:4384] "Acanthis_flammea" "Acanthis_hornemanni" "Acanthisitta_chloris" "Accipiter_badius" ...
    ##   ..$ master.spp : logi [1:4384] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ rel.spp    : logi [1:4384] NA NA NA NA NA NA ...
    ##   ..$ taxo.status: chr [1:4384] "original" "original" "original" "original" ...
    ##   ..$ genus      : chr [1:4384] "Acanthis" "Acanthis" "Acanthisitta" "Accipiter" ...
    ##   ..$ family     : chr [1:4384] "Fringillidae" "Fringillidae" "Acanthisittidae" "Accipitridae" ...
    ##   ..$ order      : chr [1:4384] "PASSERIFORMES" "PASSERIFORMES" "PASSERIFORMES" "ACCIPITRIFORMES" ...
    ##  $ metadata:'data.frame':    208 obs. of  14 variables:
    ##   ..$ code      : chr [1:208] "aerial.for" "bite" "breed.system" "brood.par" ...
    ##   ..$ orig.vname: chr [1:208] "Aerial foraging" "Biting" "Breeding system" "Brood parasitism" ...
    ##   ..$ cat       : chr [1:208] "BEHAVIORAL" "BEHAVIORAL" "BEHAVIORAL" "BEHAVIORAL" ...
    ##   ..$ n.total.  : chr [1:208] "130" "89" "965" "85" ...
    ##   ..$ n.100.    : chr [1:208] "100" "64" "97" "68" ...
    ##   ..$ descr     : chr [1:208] "Species relies on aerial foraging?" "Species responds to being touched on the beak with biting" "Which adult(s) provides the majority of care:" "Percentage of cuckoo parasitized nests of a given host species" ...
    ##   ..$ scores    : chr [1:208] "0;1" "0;1" "1;2;3;4;5" "" ...
    ##   ..$ levels    : chr [1:208] "FALSE;TRUE" "FALSE;TRUE" "Pair;Female;Male;Cooperative;Occassional" "" ...
    ##   ..$ type      : chr [1:208] "Bin" "Bin" "Cat" "Con" ...
    ##   ..$ units     : chr [1:208] "" "" "" "%" ...
    ##   ..$ ref.total : chr [1:208] "24, [80], [88], 112" "43, 76" "24, 112, (125)" "(4), 36" ...
    ##   ..$ notes     : chr [1:208] "Does species rely on flight to obtain food?  aerial foragers: 1, and if not are not aerial foragers: 0. In the case of ref. 88 "| __truncated__ "If the bird bite when researchers put the index finger of the right hand against the beak.  0: not bite, 1: bite" "1: Pair, 2: Female, 3:Male, 4: Cooperative (regularly breeds cooperatively, with non-parents providing parental care), 5: Ocass"| __truncated__ "" ...
    ##   ..$ source    : chr [1:208] "original" "original" "original" "original" ...
    ##   ..$ log       : chr [1:208] "FALSE" "FALSE" "FALSE" "FALSE" ...
