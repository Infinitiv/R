library('xml2')

xml_file <- read_xml("~/AS_ADDR_OBJ_20241111_54c0a18b-1ced-40d0-817b-42c533e909ca.xml")
records <- xml_find_all(xml_file, ".//OBJECT")
data <- data.frame(
  id = xml_attr(records, "ID"),
  object_id = xml_attr(records, "OBJECTID"),
  object_guid = xml_attr(records, "OBJECTGUID"),
  change_id = xml_attr(records, "CHANGEID"),
  name = xml_attr(records, "NAME"),
  type_name = xml_attr(records, "TYPENAME"),
  level = xml_attr(records, "LEVEL"),
  oper_type_id = xml_attr(records, "OPERTYPEID"),
  prev_id = xml_attr(records, "PREVID"),
  next_id = xml_attr(records, "NEXTID"),
  update_date = xml_attr(records, "UPDATEDATE"),
  start_date = xml_attr(records, "STARTDATE"),
  end_date = xml_attr(records, "ENDDATE"),
  is_actual = xml_attr(records, "ISACTUAL"),
  is_active = xml_attr(records, "ISACTIVE")
)
write.csv2(data, "~/AS_ADDR_OBJ.csv")

xml_file <- read_xml("~/AS_HOUSES_20241111_f6312ab5-b5f6-4d37-9f9e-b6a3e381f0e9.xml")
records <- xml_find_all(xml_file, ".//HOUSE")
data <- data.frame(
  id = xml_attr(records, "ID"),
  object_id = xml_attr(records, "OBJECTID"),
  object_guid = xml_attr(records, "OBJECTGUID"),
  change_id = xml_attr(records, "CHANGEID"),
  house_num = xml_attr(records, "HOUSENUM"),
  house_type = xml_attr(records, "HOUSETYPE"),
  oper_type_id = xml_attr(records, "OPERTYPEID"),
  prev_id = xml_attr(records, "PREVID"),
  next_id = xml_attr(records, "NEXTID"),
  update_date = xml_attr(records, "UPDATEDATE"),
  start_date = xml_attr(records, "STARTDATE"),
  end_date = xml_attr(records, "ENDDATE"),
  is_actual = xml_attr(records, "ISACTUAL"),
  is_active = xml_attr(records, "ISACTIVE")
)
write.csv2(data, "~/AS_HOUSES.csv")