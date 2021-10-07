
builder <- DBModelR::ORM()$model_builder()

Adress <- builder(
  "Adress",
  id=IntegerField(default=3),
  street=CharacterField(),
  number=IntegerField(default=3)
)
Person <- builder(
  "Person",
  name=CharacterField(default="ciel"),
  fictional=BooleanField(default=TRUE),
  height=FloatField(default=1.67),
  picture=BlobField(default=blob::as_blob("ciel.png")),
  adress=ForeignKeyField(Adress(), list(this="id", other="id"), MANY_TO_MANY),
  birthdate=DateField(default=as.POSIXct("02/09/1995"))
)
