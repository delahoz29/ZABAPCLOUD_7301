CLASS zcl_validacion_work_order_7301 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      validate_create_order        IMPORTING iv_customer_id   TYPE zde_customer_id_7301
                                             iv_technician_id TYPE zde_technician_id_7301
                                             iv_priority      TYPE zde_priority_7301
                                   RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order        IMPORTING iv_work_order_id TYPE zde_work_order_id_7301
                                             iv_status        TYPE zde_status_7301
                                   RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order        IMPORTING iv_work_order_id TYPE zde_work_order_id_7301
                                             iv_status        TYPE zde_status_7301
                                   RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status       TYPE zde_status_7301
                                             iv_priority     TYPE zde_priority_7301
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS  check_customer_exists IMPORTING iv_customer_id   TYPE zde_customer_id_7301
                                   RETURNING VALUE(rv_exists) TYPE abap_bool .

    METHODS check_technician_exists IMPORTING iv_technician_id TYPE zde_technician_id_7301
                                    RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_exists IMPORTING iv_work_order_id TYPE zde_work_order_id_7301
                               RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_history IMPORTING iv_work_order_id TYPE zde_work_order_id_7301
                                RETURNING VALUE(rv_exists) TYPE abap_bool.

CONSTANTS: c_valid_status_pending   TYPE string VALUE 'PE',
               c_valid_status_completed TYPE string VALUE 'CO',
               c_valid_priority_high    TYPE c VALUE 'A',
               c_valid_priority_low     TYPE c VALUE 'B'.


ENDCLASS.


CLASS zcl_validacion_work_order_7301 IMPLEMENTATION.


  METHOD validate_create_order.
    " Chequear si existe el cliente
    DATA(lv_customer_exists) =  check_customer_exists( iv_customer_id ).

    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Chequear si existe el técnico
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Chequear si la prioridad es valida
    IF iv_priority NE c_valid_priority_high AND iv_priority NE c_valid_priority_low .
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_update_order.
    " Validar si existe la orden de trabajo
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validar si el estado es válido
    IF iv_status NE c_valid_status_pending.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_delete_order.
    " Chequear si existe la orden de trabajo
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Chequear si el estado se encuentra en 'PE'
    IF iv_status NE c_valid_status_pending.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Chequear si la orden de trabajo ya tiene historia (i.e., Si ha sido modificada antes)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_status_and_priority.
    " Validar el valor del estado
    IF iv_status NE c_valid_status_pending AND iv_status NE c_valid_status_completed.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validar el valor de la prioridad
    IF iv_priority NE c_valid_priority_high AND iv_priority NE c_valid_priority_low.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.


  METHOD check_customer_exists.

    SELECT SINGLE FROM ztcustomer_7301
    FIELDS  customer_id
    WHERE customer_id EQ @iv_customer_id
    INTO @DATA(lv_customer).

    rv_exists = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD check_order_exists.
    SELECT SINGLE FROM ztworkorder_7301
   FIELDS  work_order_id
   WHERE work_order_id EQ @iv_work_order_id
   INTO @DATA(lv_work_order).

    rv_exists = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD check_order_history.
    SELECT SINGLE FROM ztworkorhist7301
      FIELDS history_id
      WHERE work_order_id EQ @iv_work_order_id
      INTO @DATA(lv_work_order_his).

    rv_exists = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_false ELSE abap_true ).

  ENDMETHOD.

  METHOD check_technician_exists.

    SELECT SINGLE FROM zttechnician7301
    FIELDS  technician_id
    WHERE technician_id EQ @iv_technician_id
    INTO @DATA(lv_technician).

    rv_exists = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

ENDCLASS.
