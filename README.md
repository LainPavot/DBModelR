
# DBModelR

The DBModelR is an ORM package (Object-Relationnal Mapping). This ORM
aims to abstract the database layer in R programs, and provide simple
objects to create, load, update and remove data fom the database
transparently, using only R Objects.

It’s built atop of RSQLite and R-DBI, but may include other DBMS like
PostGreSQL, or MySQL.

You can install the latest released version from CRAN with:

install.packages(“DBModelR”)

Or install the latest development version from GitHub with:

devtools::install\_github(“LainPavot/DBModelR”)

# Basic usage

``` r
library("RSQLite")
library("DBModelR")

models <- list(
    person=ModelDefinition(
        table="person",
        fields=list(
            name="TEXT",
            family_name="TEXT"
        ), many=list("adress")
        ## One person possibly has multiple adress. Or none...
        ## "many" fields defines the creation of a linkage table
        ## (many_to_many or one_to_many).
        ## "one" fields defines a fk field enforced by a foreign key
        ## restriction (one_to_one)
        ## "many" fields mustn't be duplicated. So, "adress" must not
        ## reference the "person" table.
    ),
    adress=ModelDefinition(
        table="adress",
        fields=list(
            number="INTEGER",
            street="TEXT"
        )
    )
)

DB_PATH <- "./people.sqlite"
if (file.exists(DB_PATH)) {
    file.remove(DB_PATH)
}
orm <- ORM(connection_params=list(DB_PATH), model_definitions=models)

## like this, we'll see the requests generated by the orm
print(orm$create_database())
```

    ## [[1]]
    ## [1] "CREATE TABLE  person (id INTEGER PRIMARY KEY, name TEXT, family_name TEXT)"
    ## 
    ## [[2]]
    ## [1] "CREATE TABLE  adress (id INTEGER PRIMARY KEY, number INTEGER, street TEXT)"
    ## 
    ## [[3]]
    ## [1] "CREATE TABLE  adress_person (id INTEGER PRIMARY KEY, adress_id INTEGER, person_id INTEGER, FOREIGN KEY (person_id) REFERENCES person (id), FOREIGN KEY (adress_id) REFERENCES adress (id))"

``` r
## the tables has been generated, fks and their restrictions has been
## defined and linked table has been created if necessary

## we've created a person who's name is Alice Smith.
## Alice has been saved into the database.
alice <- orm$person(name="Alice", family_name="smith")$save()

## Alice has been successfully added to and loaded from the database.
print(orm$person()$load_by(name="Alice"))
```

    ## [[1]]
    ## <person [id: 1]>: 
    ##   [name: "Alice"]
    ##   [family_name: "smith"]
    ## 

``` r
## The id is 1, because it's the first person to be inserted into the
## "person" table.
## The orm has generated an "INSERT" query.
print(alice$get_id())
```

    ## [1] 1

``` r
## He's a boy, so he's changed his name to "Bob". Suits him better.
## Fields must always be setted with "model$set_field_name(value)".
## otherwise the orm will not see the modifications, and will not save
## them in the database.
bob <- alice$set_name("bob")$save()

## still 1. Because the orm did not add new database entry.
## The orm has generated an "UPDATE" query.
print(bob$get_id())
```

    ## [1] 1

``` r
## prints an empty list()
## no adress has been assigned to him for the moment.
print(bob$get_adress())
```

    ## list()

``` r
## let's give him a home
bob$add_adress(
    ## the orm sanitizes the user's inputs and prevent sql injections.
    orm$adress(number=42, street="Second street ; -- drop table person")
)
```

    ## <person [id: 1]>: 
    ##   [name: "bob"]
    ##   [family_name: "smith"]
    ## 

``` r
## The orm detects that the adress object assigned to bob is not saved
## yet in the database.
## So, the orm will register the adress, and then create a link between
## bob and the adress through a linkage table.
bob$save()
```

    ## <person [id: 1]>: 
    ##   [name: "bob"]
    ##   [family_name: "smith"]
    ## 

``` r
## now Bob is happy because he has an adress
## prints: list(<adress id: 1> ...) etc.
print(adress <- bob$get_adress())
```

    ## [[1]]
    ## <adress [id: 1]>: 
    ##   [number: 42]
    ##   [street: "Second street ; -- drop table person"]
    ## 

``` r
## the street name is still somewat strange...
## let's make it less strange
bob$get_adress(
    ## we select the adress with a strange name...
    street="Second street ; -- drop table person"

## we set a more... usual name. And we save it (the adress).
)[[1]]$set_street("Second street")$save()
```

    ## <adress [id: 1]>: 
    ##   [number: 42]
    ##   [street: "Second street"]
    ## 

``` r
print(bob$get_adress())
```

    ## [[1]]
    ## <adress [id: 1]>: 
    ##   [number: 42]
    ##   [street: "Second street"]
    ## 

``` r
## never forget to disconnect when your're finished!
orm$disconnect()
```

    ## [1] TRUE

``` r
## not to forget, there's a little trick:
## this call orm$connect()
## and at the end of the block, it calls orm$disconnect()
## so you never foget to disconnect from the database.
orm <- ORM(connection_params=list(DB_PATH), model_definitions=models, connect=FALSE)
orm$with_connection({
    bob <- orm$person()$load_by(name="bob")$first()
    bob$add_adress(
        ## he's has a second residence
        orm$adress(number=2, street="the squirel's path")
    )$save()
    print(bob$get_adress())

    ## finily he decided to live in his second house, and sold the
    ## first one.
    print(bob$get_adress(street="Second street"))
    bob$remove_adress(bob$get_adress(street="Second street")$first())
    bob$save()
    print(bob$get_adress())

})
```

    ## [[1]]
    ## <adress [id: 1]>: 
    ##   [number: 42]
    ##   [street: "Second street"]
    ##   
    ## [[2]]
    ## <adress [id: 2]>: 
    ##   [number: 2]
    ##   [street: "the squirel's path"]
    ##   
    ## [[1]]
    ## <adress [id: 1]>: 
    ##   [number: 42]
    ##   [street: "Second street"]
    ##   
    ## [[1]]
    ## <adress [id: 2]>: 
    ##   [number: 2]
    ##   [street: "the squirel's path"]
    ## 

    ## [[1]]
    ## <adress [id: 2]>: 
    ##   [number: 2]
    ##   [street: "the squirel's path"]
    ## 

``` r
## now, you're disconnected from the database.
file.remove(DB_PATH)
```

    ## [1] TRUE

# Context

This package has been initialized in the ANR RHU project ChoPIn
(Cholesterol Personnelized Innovations), at the “plateforme
d’exploration du metabolisme” (Metabolism exploration plateform -
PFEM), at INRAE of Clermont-Theix in France.

The ORM was a part of the “XSeeker” tool that I was coding, and I
decided to make a more compleet tool. So on my free time, I created this
package, largly inspired from the original code.

# Contributor Code of Conduct

As contributors and maintainers of this project, we pledge to respect
all people who contribute through reporting issues, posting feature
requests, updating documentation, submitting pull requests or patches,
and other activities.

We are committed to making participation in this project a
harassment-free experience for everyone, regardless of level of
experience, gender, gender identity and expression, sexual orientation,
disability, personal appearance, body size, ethnicity, age, or religion.

Examples of unacceptable behavior by participants include the use of
sexual language or imagery, derogatory comments or personal attacks,
trolling, public or private harassment, insults, or other unprofessional
conduct.

Project maintainers have the right and responsibility to remove, edit,
or reject comments, commits, code, wiki edits, issues, and other
contributions that are not aligned to this Code of Conduct. Project
maintainers who do not follow the Code of Conduct may be removed from
the project team.

Instances of abusive, harassing, or otherwise unacceptable behavior may
be reported by opening an issue or contacting one or more of the project
maintainers.

This Code of Conduct is adapted from the [Contributor
Covenant](http:contributor-covenant.org), version 1.0.0, available at
<http://contributor-covenant.org/version/1/0/0/>.
