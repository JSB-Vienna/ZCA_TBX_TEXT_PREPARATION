*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE add_table_tag.
  IF me->ms_text_font-doc_class EQ c_docclass_htm.
    <lv_txt_fld> = &1.
    APPEND <ls_text_line> TO et_text.
  ENDIF.
END-OF-DEFINITION.
