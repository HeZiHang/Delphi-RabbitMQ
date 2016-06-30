unit uRabbitMQ;

interface

const
  LIBFILE = 'rabbitmq.4.dll';

const
  POOL_TABLE_SIZE = 16;

const
  HEADER_SIZE = 7;
  FOOTER_SIZE = 1;
  (* 7 bytes up front, then payload, then 1 byte footer *)

  AMQP_PSEUDOFRAME_PROTOCOL_HEADER = 'A';

type
  ssize_t = NativeInt;
  size_t = NativeInt;

{$REGION 'amqp_time.h'}

const
  AMQP_MS_PER_S = 1000;
  AMQP_US_PER_MS = 1000;
  AMQP_NS_PER_S = 1000000000;
  AMQP_NS_PER_MS = 1000000;
  AMQP_NS_PER_US = 1000;

  (* This represents a point in time in reference to a monotonic clock.
    *
    * The internal representation is ns, relative to the monotonic clock.
    *
    * There are two 'special' values:
    * - 0: means 'this instant', its meant for polls with a 0-timeout, or
    *   non-blocking option
    * - UINT64_MAX: means 'at infinity', its mean for polls with an infinite
    *   timeout
  *)
type

  (*
    * Structure used in select() call, taken from the BSD file sys/time.h.
  *)
  timeval = record
    tv_sec: UInt32; (* seconds *)
    tv_usec: UInt32; (* and microseconds *)
  end;

  ptimeval = ^timeval;

  amqp_time_t_ = record
    time_point_ns: UInt64;
  end;

  amqp_time_t = amqp_time_t_;
  pamqp_time_t = ^amqp_time_t;
{$ENDREGION}

{$REGION 'amqp_socket.h'}
  (* *
    * An abstract socket interface.
  *)

  amqp_socket_flag_enum = (AMQP_SF_NONE = 0, AMQP_SF_MORE = 1, AMQP_SF_POLLIN = 2, AMQP_SF_POLLOUT = 4, AMQP_SF_POLLERR = 8);

  amqp_socket_close_enum = (AMQP_SC_NONE = 0, AMQP_SC_FORCE = 1);

  // function amqp_os_socket_error: integer; cdecl;
  //
  // function amqp_os_socket_close(sockfd: integer): integer; cdecl;

  (* Socket callbacks. *)
  amqp_socket_send_fn = function(a: Pointer; const b: Pointer; c: size_t; d: integer): ssize_t;
  amqp_socket_recv_fn = function(a: Pointer; b: Pointer; c: size_t; d: integer): ssize_t;
  amqp_socket_open_fn = function(a: Pointer; const b: PAnsiChar; c: integer; d: ptimeval): integer;
  amqp_socket_close_fn = function(a: Pointer; b: amqp_socket_close_enum): integer;
  amqp_socket_get_sockfd_fn = function: integer;
  amqp_socket_delete_fn = procedure(a: Pointer);

  (* * V-table for amqp_socket_t *)
  amqp_socket_class_t = record
    send: amqp_socket_send_fn;
    recv: amqp_socket_recv_fn;
    open: amqp_socket_open_fn;
    close: amqp_socket_close_fn;
    get_sockfd: amqp_socket_get_sockfd_fn;
    delete: amqp_socket_delete_fn;
  end;

  pamqp_socket_class_t = ^amqp_socket_class_t;
  (* * Abstract base class for amqp_socket_t *)

  pamqp_socket_t = ^amqp_socket_t_;

  amqp_socket_t_ = record
    klass: pamqp_socket_class_t;
  end;

  amqp_socket_t = amqp_socket_t_;
{$ENDREGION}
{$REGION 'unsort'}
  amqp_boolean_t = integer;

  (* *
    * Bitmask for flags
    *
    * \since v0.1
  *)
  amqp_flags_t = UInt32;

  (* *
    * Channel type
    *
    * \since v0.1
  *)
  amqp_channel_t = UInt16;

  (* *
    * Buffer descriptor
    *
    * \since v0.1
  *)
  pamqp_bytes_t = ^amqp_bytes_t_;

  amqp_bytes_t_ = record
    len: size_t; (* *< length of the buffer in bytes *)
    bytes: Pointer; (* *< pointer to the beginning of the buffer *)
  end;

  amqp_bytes_t = amqp_bytes_t_;

  (* *
    * Method number
    *
    * \since v0.1
  *)
  pamqp_method_number_t = ^amqp_method_number_t;
  amqp_method_number_t = UInt32;

  (* *
    * A list of allocation blocks
    *
    * \since v0.1
  *)
  amqp_pool_blocklist_t_ = record
    num_blocks: integer; (* *< Number of blocks in the block list *)
    blocklist: PPointer;
  end;

  amqp_pool_blocklist_t = amqp_pool_blocklist_t_;
  (* *
    * A memory pool
    *
    * \since v0.1
  *)
  pamqp_pool_t = ^amqp_pool_t_;

  amqp_pool_t_ = record
    pagesize: size_t; (* *< the size of the page in bytes.
      *  allocations less than or equal to this size are
      *    allocated in the pages block list
      *  allocations greater than this are allocated in their
      *   own block in the large_blocks block list *)

    pages: amqp_pool_blocklist_t; (* *< blocks that are the size of pagesize *)
    large_blocks: amqp_pool_blocklist_t; (* *< allocations larger than the pagesize *)

    next_page: integer; (* *< an index to the next unused page block *)
    alloc_block: PAnsiChar; (* *< pointer to the current allocation block *)
    alloc_used: size_t; (* *< number of bytes in the current allocation block that has been used *)
  end;

  amqp_pool_t = amqp_pool_t_;

  (* *
    * Decimal data type
    *
    * \since v0.1
  *)
  amqp_decimal_t_ = record
    decimals: UInt8; (* *< the location of the decimal point *)
    value: UInt32; (* *< the value before the decimal point is applied *)
  end;

  amqp_decimal_t = amqp_decimal_t_;

  (* *
    * An AMQP Field Array
    *
    * A repeated set of field values, all must be of the same type
    *
    * \since v0.1
  *)
  pamqp_field_value_t_ = ^amqp_field_value_t_;

  amqp_array_t_ = record
    num_entries: integer; (* *< Number of entries in the table *)
    entries: pamqp_field_value_t_; (* *< linked list of field values *)
  end;

  amqp_array_t = amqp_array_t_;

  (* *
    * AMQP field table
    *
    * An AMQP field table is a set of key-value pairs.
    * A key is a UTF-8 encoded string up to 128 bytes long, and are not null
    * terminated.
    * A value can be one of several different datatypes. \sa amqp_field_value_kind_t
    *
    * \sa amqp_table_entry_t
    *
    * \since v0.1
  *)
  pamqp_table_t = ^amqp_table_t_;

  pamqp_table_entry_t_ = ^amqp_table_entry_t_;

  amqp_table_t_ = record
    num_entries: integer; (* *< length of entries array *)
    entries: pamqp_table_entry_t_; (* *< an array of table entries *)
  end;

  amqp_table_t = amqp_table_t_;

  (* *
    * A field table value
    *
    * \since v0.1
  *)
  amqp_field_value_t_ = record
    kind: UInt8;

    value: record
      case integer of
        0:
          (&boolean: amqp_boolean_t);
        1:
          (i8: Int8);
        2:
          (u8: UInt8);
        3:
          (i16: Int16);
        4:
          (u16: UInt16);
        5:
          (i32: Int32);
        6:
          (u32: UInt32);
        7:
          (i64: Int64);
        8:
          (u64: UInt64);
        9:
          (f32: Single);
        10:
          (f64: Double);
        11:
          (decimal: amqp_decimal_t);
        12:
          (bytes: amqp_bytes_t);
        13:
          (table: amqp_table_t);
        14:
          (&array: amqp_array_t);
    end;
  end;

  amqp_field_value_t = amqp_field_value_t_;

  (* *
    * An entry in a field-table
    *
    * \sa amqp_table_encode(), amqp_table_decode(), amqp_table_clone()
    *
    * \since v0.1
  *)
  amqp_table_entry_t_ = record
    key: amqp_bytes_t; (* *< the table entry key. Its a null-terminated UTF-8 string,
      * with a maximum size of 128 bytes *)
    value: amqp_field_value_t; (* *< the table entry values *)
  end;

  amqp_table_entry_t = amqp_table_entry_t_;

  pamqp_pool_table_entry_t = ^amqp_pool_table_entry_t_;
  pamqp_pool_table_entry_t_ = pamqp_pool_table_entry_t;

  amqp_pool_table_entry_t_ = record
    next: pamqp_pool_table_entry_t_;
    pool: amqp_pool_t;
    channel: amqp_channel_t;
  end;

  amqp_pool_table_entry_t = amqp_pool_table_entry_t_;

  pamqp_link_t_ = ^amqp_link_t_;
  pamqp_link_t = pamqp_link_t_;

  amqp_link_t_ = record
    next: pamqp_link_t_;
    data: Pointer;
  end;

  amqp_link_t = amqp_link_t_;

  (* *
    * An amqp method
    *
    * \since v0.1
  *)
  pamqp_method_t = ^amqp_method_t_;

  amqp_method_t_ = record
    id: amqp_method_number_t; (* *< the method id number *)
    decoded: Pointer; (* *< pointer to the decoded method,
      *    cast to the appropriate type to use *)
  end;

  amqp_method_t = amqp_method_t_;

  (* *
    * Response type
    *
    * \since v0.1
  *)
  amqp_response_type_enum_ = (AMQP_RESPONSE_NONE = 0, (* the library got an EOF from the socket *)
    AMQP_RESPONSE_NORMAL, (* response normal, the RPC completed successfully *)
    AMQP_RESPONSE_LIBRARY_EXCEPTION, (* library error, an error occurred in the library , examine the library_error *)
    AMQP_RESPONSE_SERVER_EXCEPTION (* server exception, the broker returned an error, check replay *)
    );
  amqp_response_type_enum = amqp_response_type_enum_;

  (* *
    * Reply from a RPC method on the broker
    *
    * \since v0.1
  *)
  amqp_rpc_reply_t_ = record
    reply_type: amqp_response_type_enum; (* the reply type:
      * - AMQP_RESPONSE_NORMAL - the RPC completed successfully
      * - AMQP_RESPONSE_SERVER_EXCEPTION - the broker returned
      *     an exception, check the reply field
      * - AMQP_RESPONSE_LIBRARY_EXCEPTION - the library
      *    encountered an error, check the library_error field
    *)
    reply: amqp_method_t; (* *< in case of AMQP_RESPONSE_SERVER_EXCEPTION this
      * field will be set to the method returned from the broker *)
    library_error: integer; (* *< in case of AMQP_RESPONSE_LIBRARY_EXCEPTION this
      *    field will be set to an error code. An error
      *     string can be retrieved using amqp_error_string *)
  end;

  amqp_rpc_reply_t = amqp_rpc_reply_t_;

  amqp_connection_state_enum_ = (CONNECTION_STATE_IDLE = 0, CONNECTION_STATE_INITIAL, CONNECTION_STATE_HEADER, CONNECTION_STATE_BODY);
  amqp_connection_state_enum = amqp_connection_state_enum_;

  amqp_connection_state_t = ^amqp_connection_state_t_;

  amqp_connection_state_t_ = record
    pool_table: array [0 .. Pred(POOL_TABLE_SIZE)] of pamqp_pool_table_entry_t;
    state: amqp_connection_state_enum;
    channel_max: integer;
    frame_max: integer;
    (* Heartbeat interval in seconds. If this is <= 0, then heartbeats are not
      * enabled, and next_recv_heartbeat and next_send_heartbeat are set to
      * infinite *)
    heartbeat: integer;
    next_recv_heartbeat: amqp_time_t;
    next_send_heartbeat: amqp_time_t;
    (* buffer for holding frame headers.  Allows us to delay allocating
      * the raw frame buffer until the type, channel, and size are all known
    *)
    header_buffer: array [0 .. Pred(HEADER_SIZE + 1)] of char;
    inbound_buffer: amqp_bytes_t;
    inbound_offset: size_t;
    target_size: size_t;
    outbound_buffer: amqp_bytes_t;
    socket: pamqp_socket_t;
    sock_inbound_buffer: amqp_bytes_t;
    sock_inbound_offset: size_t;
    sock_inbound_limit: size_t;
    first_queued_frame: pamqp_link_t;
    last_queued_frame: pamqp_link_t;
    most_recent_api_result: amqp_rpc_reply_t;
    server_properties: amqp_table_t;
    client_properties: amqp_table_t;
    properties_pool: amqp_pool_t;
  end;

{$ENDREGION}
{$REGION 'amqp_framing.h'}

const
  AMQP_PROTOCOL_VERSION_MAJOR = 0; (* *< AMQP protocol version major *)
  AMQP_PROTOCOL_VERSION_MINOR = 9; (* *< AMQP protocol version minor *)
  AMQP_PROTOCOL_VERSION_REVISION = 1; (* *< AMQP protocol version revision *)
  AMQP_PROTOCOL_PORT = 5672; (* *< Default AMQP Port *)
  AMQP_FRAME_METHOD = 1; (* *< Constant: FRAME-METHOD *)
  AMQP_FRAME_HEADER = 2; (* *< Constant: FRAME-HEADER *)
  AMQP_FRAME_BODY = 3; (* *< Constant: FRAME-BODY *)
  AMQP_FRAME_HEARTBEAT = 8; (* *< Constant: FRAME-HEARTBEAT *)
  AMQP_FRAME_MIN_SIZE = 4096; (* *< Constant: FRAME-MIN-SIZE *)
  AMQP_FRAME_END = 206; (* *< Constant: FRAME-END *)
  AMQP_REPLY_SUCCESS = 200; (* *< Constant: REPLY-SUCCESS *)
  AMQP_CONTENT_TOO_LARGE = 311; (* *< Constant: CONTENT-TOO-LARGE *)
  AMQP_NO_ROUTE = 312; (* *< Constant: NO-ROUTE *)
  AMQP_NO_CONSUMERS = 313; (* *< Constant: NO-CONSUMERS *)
  AMQP_ACCESS_REFUSED = 403; (* *< Constant: ACCESS-REFUSED *)
  AMQP_NOT_FOUND = 404; (* *< Constant: NOT-FOUND *)
  AMQP_RESOURCE_LOCKED = 405; (* *< Constant: RESOURCE-LOCKED *)
  AMQP_PRECONDITION_FAILED = 406; (* *< Constant: PRECONDITION-FAILED *)
  AMQP_CONNECTION_FORCED = 320; (* *< Constant: CONNECTION-FORCED *)
  AMQP_INVALID_PATH = 402; (* *< Constant: INVALID-PATH *)
  AMQP_FRAME_ERROR = 501; (* *< Constant: FRAME-ERROR *)
  AMQP_SYNTAX_ERROR = 502; (* *< Constant: SYNTAX-ERROR *)
  AMQP_COMMAND_INVALID = 503; (* *< Constant: COMMAND-INVALID *)
  AMQP_CHANNEL_ERROR = 504; (* *< Constant: CHANNEL-ERROR *)
  AMQP_UNEXPECTED_FRAME = 505; (* *< Constant: UNEXPECTED-FRAME *)
  AMQP_RESOURCE_ERROR = 506; (* *< Constant: RESOURCE-ERROR *)
  AMQP_NOT_ALLOWED = 530; (* *< Constant: NOT-ALLOWED *)
  AMQP_NOT_IMPLEMENTED = 540; (* *< Constant: NOT-IMPLEMENTED *)
  AMQP_INTERNAL_ERROR = 541; (* *< Constant: INTERNAL-ERROR *)

  (* Function prototypes. *)

  (* *
    * Get constant name string from constant
    *
    * @param [in] constantNumber constant to get the name of
    * @returns string describing the constant. String is managed by
    *           the library and should not be free()'d by the program
  *)
function amqp_constant_name(constantNumber: integer): PAnsiChar; cdecl;

(* *
  * Checks to see if a constant is a hard error
  *
  * A hard error occurs when something severe enough
  * happens that the connection must be closed.
  *
  * @param [in] constantNumber the error constant
  * @returns true if its a hard error, false otherwise
*)

function amqp_constant_is_hard_error(constantNumber: integer): amqp_boolean_t; cdecl;

(* *
  * Get method name string from method number
  *
  * @param [in] methodNumber the method number
  * @returns method name string. String is managed by the library
  *           and should not be freed()'d by the program
*)
function amqp_method_name(methodNumber: amqp_method_number_t): PAnsiChar; cdecl;

(* *
  * Check whether a method has content
  *
  * A method that has content will receive the method frame
  * a properties frame, then 1 to N body frames
  *
  * @param [in] methodNumber the method number
  * @returns true if method has content, false otherwise
*)

function amqp_method_has_content(methodNumber: amqp_method_number_t): amqp_boolean_t; cdecl;

(* *
  * Decodes a method from AMQP wireformat
  *
  * @param [in] methodNumber the method number for the decoded parameter
  * @param [in] pool the memory pool to allocate the decoded method from
  * @param [in] encoded the encoded byte string buffer
  * @param [out] decoded pointer to the decoded method struct
  * @returns 0 on success, an error code otherwise
*)

function amqp_decode_method(methodNumber: amqp_method_number_t; pool: pamqp_pool_t; encoded: amqp_bytes_t; var decoded: Pointer): integer; cdecl;

(* *
  * Decodes a header frame properties structure from AMQP wireformat
  *
  * @param [in] class_id the class id for the decoded parameter
  * @param [in] pool the memory pool to allocate the decoded properties from
  * @param [in] encoded the encoded byte string buffer
  * @param [out] decoded pointer to the decoded properties struct
  * @returns 0 on success, an error code otherwise
*)

function amqp_decode_properties(class_id: UInt16; pool: pamqp_pool_t; encoded: amqp_bytes_t; var decoded: Pointer): integer; cdecl;

(* *
  * Encodes a method structure in AMQP wireformat
  *
  * @param [in] methodNumber the method number for the decoded parameter
  * @param [in] decoded the method structure (e.g., amqp_connection_start_t)
  * @param [in] encoded an allocated byte buffer for the encoded method
  *              structure to be written to. If the buffer isn't large enough
  *              to hold the encoded method, an error code will be returned.
  * @returns 0 on success, an error code otherwise.
*)

function amqp_encode_method(methodNumber: amqp_method_number_t; decoded: pinteger; encoded: amqp_bytes_t): integer; cdecl;

(* *
  * Encodes a properties structure in AMQP wireformat
  *
  * @param [in] class_id the class id for the decoded parameter
  * @param [in] decoded the properties structure (e.g., amqp_basic_properties_t)
  * @param [in] encoded an allocated byte buffer for the encoded properties to written to.
  *              If the buffer isn't large enough to hold the encoded method, an
  *              an error code will be returned
  * @returns 0 on success, an error code otherwise.
*)

function amqp_encode_properties(class_id: UInt16; decoded: Pointer; encoded: amqp_bytes_t): integer; cdecl;

(* Method field records. *)
const
  AMQP_CONNECTION_START_METHOD: amqp_method_number_t = $000A000A; (* *< connection.start method id @internal 10, 10; 655370 *)

  (* * connection.start method fields *)
type
  amqp_connection_start_t_ = record
    version_major: UInt8; (* *< version-major *)
    version_minor: UInt8; (* *< version-minor *)
    server_properties: amqp_table_t; (* *< server-properties *)
    mechanisms: amqp_bytes_t; (* *< mechanisms *)
    locales: amqp_bytes_t; (* *< locales *)
  end;

  amqp_connection_start_t = amqp_connection_start_t_;
  pamqp_connection_start_t = ^amqp_connection_start_t;

const
  AMQP_CONNECTION_START_OK_METHOD: amqp_method_number_t = $000A000B; (* *< connection.start-ok method id @internal 10, 11; 655371 *)

  (* * connection.start-ok method fields *)
type
  amqp_connection_start_ok_t_ = record
    client_properties: amqp_table_t; (* *< client-properties *)
    mechanism: amqp_bytes_t; (* *< mechanism *)
    response: amqp_bytes_t; (* *< response *)
    locale: amqp_bytes_t; (* *< locale *)
  end;

  amqp_connection_start_ok_t = amqp_connection_start_ok_t_;
  pamqp_connection_start_ok_t = ^amqp_connection_start_ok_t;

const
  AMQP_CONNECTION_SECURE_METHOD: amqp_method_number_t = $000A0014; (* *< connection.secure method id @internal 10, 20; 655380 *)

  (* * connection.secure method fields *)
type
  amqp_connection_secure_t_ = record
    challenge: amqp_bytes_t; (* *< challenge *)
  end;

  amqp_connection_secure_t = amqp_connection_secure_t_;
  pamqp_connection_secure_t = ^amqp_connection_secure_t;

const
  AMQP_CONNECTION_SECURE_OK_METHOD: amqp_method_number_t = $000A0015; (* *< connection.secure-ok method id @internal 10, 21; 655381 *)

  (* * connection.secure-ok method fields *)
type
  amqp_connection_secure_ok_t_ = record
    response: amqp_bytes_t; (* *< response *)
  end;

  amqp_connection_secure_ok_t = amqp_connection_secure_ok_t_;
  pamqp_connection_secure_ok_t = ^amqp_connection_secure_ok_t;

const
  AMQP_CONNECTION_TUNE_METHOD: amqp_method_number_t = $000A001E; (* *< connection.tune method id @internal 10, 30; 655390 *)

  (* * connection.tune method fields *)
type
  amqp_connection_tune_t_ = record
    channel_max: UInt16; (* *< channel-max *)
    frame_max: UInt32; (* *< frame-max *)
    heartbeat: UInt16; (* *< heartbeat *)
  end;

  amqp_connection_tune_t = amqp_connection_tune_t_;
  pamqp_connection_tune_t = ^amqp_connection_tune_t;

const
  AMQP_CONNECTION_TUNE_OK_METHOD: amqp_method_number_t = $000A001F; (* *< connection.tune-ok method id @internal 10, 31; 655391 *)

  (* * connection.tune-ok method fields *)
type
  amqp_connection_tune_ok_t_ = record
    channel_max: UInt16; (* *< channel-max *)
    frame_max: UInt32; (* *< frame-max *)
    heartbeat: UInt16; (* *< heartbeat *)
  end;

  amqp_connection_tune_ok_t = amqp_connection_tune_ok_t_;
  pamqp_connection_tune_ok_t = ^amqp_connection_tune_ok_t;

const
  AMQP_CONNECTION_OPEN_METHOD: amqp_method_number_t = $000A0028; (* *< connection.open method id @internal 10, 40; 655400 *)

  (* * connection.open method fields *)
type
  amqp_connection_open_t_ = record
    virtual_host: amqp_bytes_t; (* *< virtual-host *)
    capabilities: amqp_bytes_t; (* *< capabilities *)
    insist: amqp_boolean_t; (* *< insist *)
  end;

  amqp_connection_open_t = amqp_connection_open_t_;
  pamqp_connection_open_t = ^amqp_connection_open_t;

const
  AMQP_CONNECTION_OPEN_OK_METHOD: amqp_method_number_t = $000A0029; (* *< connection.open-ok method id @internal 10, 41; 655401 *)

  (* * connection.open-ok method fields *)
type
  amqp_connection_open_ok_t_ = record
    known_hosts: amqp_bytes_t; (* *< known-hosts *)
  end;

  amqp_connection_open_ok_t = amqp_connection_open_ok_t_;
  pamqp_connection_open_ok_t = ^amqp_connection_open_ok_t;

const
  AMQP_CONNECTION_CLOSE_METHOD: amqp_method_number_t = $000A0032; (* *< connection.close method id @internal 10, 50; 655410 *)

  (* * connection.close method fields *)
type
  amqp_connection_close_t_ = record
    reply_code: UInt16; (* *< reply-code *)
    reply_text: amqp_bytes_t; (* *< reply-text *)
    class_id: UInt16; (* *< class-id *)
    method_id: UInt16; (* *< method-id *)
  end;

  amqp_connection_close_t = amqp_connection_close_t_;
  pamqp_connection_close_t = ^amqp_connection_close_t;

const
  AMQP_CONNECTION_CLOSE_OK_METHOD: amqp_method_number_t = $000A0033; (* *< connection.close-ok method id @internal 10, 51; 655411 *)

  (* * connection.close-ok method fields *)
type
  amqp_connection_close_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_connection_close_ok_t = amqp_connection_close_ok_t_;
  pamqp_connection_close_ok_t = ^amqp_connection_close_ok_t;

const
  AMQP_CONNECTION_BLOCKED_METHOD: amqp_method_number_t = $000A003C; (* *< connection.blocked method id @internal 10, 60; 655420 *)

  (* * connection.blocked method fields *)
type
  amqp_connection_blocked_t_ = record
    reason: amqp_bytes_t; (* *< reason *)
  end;

  amqp_connection_blocked_t = amqp_connection_blocked_t_;
  pamqp_connection_blocked_t = ^amqp_connection_blocked_t;

const
  AMQP_CONNECTION_UNBLOCKED_METHOD: amqp_method_number_t = $000A003D; (* *< connection.unblocked method id @internal 10, 61; 655421 *)

  (* * connection.unblocked method fields *)
type
  amqp_connection_unblocked_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_connection_unblocked_t = amqp_connection_unblocked_t_;
  pamqp_connection_unblocked_t = ^amqp_connection_unblocked_t;

const
  AMQP_CHANNEL_OPEN_METHOD: amqp_method_number_t = $0014000A; (* *< channel.open method id @internal 20, 10; 1310730 *)

  (* * channel.open method fields *)
type
  amqp_channel_open_t_ = record
    out_of_band: amqp_bytes_t; (* *< out-of-band *)
  end;

  amqp_channel_open_t = amqp_channel_open_t_;
  pamqp_channel_open_t = ^amqp_channel_open_t;

const
  AMQP_CHANNEL_OPEN_OK_METHOD: amqp_method_number_t = $0014000B; (* *< channel.open-ok method id @internal 20, 11; 1310731 *)

  (* * channel.open-ok method fields *)
type
  amqp_channel_open_ok_t_ = record
    channel_id: amqp_bytes_t; (* *< channel-id *)
  end;

  amqp_channel_open_ok_t = amqp_channel_open_ok_t_;
  pamqp_channel_open_ok_t = ^amqp_channel_open_ok_t_;

const
  AMQP_CHANNEL_FLOW_METHOD: amqp_method_number_t = $00140014; (* *< channel.flow method id @internal 20, 20; 1310740 *)

  (* * channel.flow method fields *)
type
  amqp_channel_flow_t_ = record
    active: amqp_boolean_t; (* *< active *)
  end;

  amqp_channel_flow_t = amqp_channel_flow_t_;
  pamqp_channel_flow_t = ^amqp_channel_flow_t;

const
  AMQP_CHANNEL_FLOW_OK_METHOD: amqp_method_number_t = $00140015; (* *< channel.flow-ok method id @internal 20, 21; 1310741 *)

  (* * channel.flow-ok method fields *)
type
  amqp_channel_flow_ok_t_ = record
    active: amqp_boolean_t; (* *< active *)
  end;

  amqp_channel_flow_ok_t = amqp_channel_flow_ok_t_;
  pamqp_channel_flow_ok_t = ^amqp_channel_flow_ok_t;

const
  AMQP_CHANNEL_CLOSE_METHOD: amqp_method_number_t = $00140028; (* *< channel.close method id @internal 20, 40; 1310760 *)

  (* * channel.close method fields *)
type
  amqp_channel_close_t_ = record
    reply_code: UInt16; (* *< reply-code *)
    reply_text: amqp_bytes_t; (* *< reply-text *)
    class_id: UInt16; (* *< class-id *)
    method_id: UInt16; (* *< method-id *)
  end;

  amqp_channel_close_t = amqp_channel_close_t_;
  pamqp_channel_close_t = ^amqp_channel_close_t;

const
  AMQP_CHANNEL_CLOSE_OK_METHOD: amqp_method_number_t = $00140029; (* *< channel.close-ok method id @internal 20, 41; 1310761 *)

  (* * channel.close-ok method fields *)
type
  amqp_channel_close_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_channel_close_ok_t = amqp_channel_close_ok_t_;
  pamqp_channel_close_ok_t = ^amqp_channel_close_ok_t;

const
  AMQP_ACCESS_REQUEST_METHOD: amqp_method_number_t = $001E000A; (* *< access.request method id @internal 30, 10; 1966090 *)

  (* * access.request method fields *)
type
  amqp_access_request_t_ = record
    realm: amqp_bytes_t; (* *< realm *)
    exclusive: amqp_boolean_t; (* *< exclusive *)
    passive: amqp_boolean_t; (* *< passive *)
    active: amqp_boolean_t; (* *< active *)
    write: amqp_boolean_t; (* *< write *)
    read: amqp_boolean_t; (* *< read *)
  end;

  amqp_access_request_t = amqp_access_request_t_;
  pamqp_access_request_t = ^amqp_access_request_t;

const
  AMQP_ACCESS_REQUEST_OK_METHOD: amqp_method_number_t = $001E000B; (* *< access.request-ok method id @internal 30, 11; 1966091 *)

  (* * access.request-ok method fields *)
type
  amqp_access_request_ok_t_ = record
    ticket: UInt16; (* *< ticket *)
  end;

  amqp_access_request_ok_t = amqp_access_request_ok_t_;
  pamqp_access_request_ok_t = ^amqp_access_request_ok_t;

const
  AMQP_EXCHANGE_DECLARE_METHOD: amqp_method_number_t = $0028000A; (* *< exchange.declare method id @internal 40, 10; 2621450 *)

  (* * exchange.declare method fields *)
type
  amqp_exchange_declare_t_ = record
    ticket: UInt16; (* *< ticket *)
    exchange: amqp_bytes_t; (* *< exchange *)

    &type: amqp_bytes_t; (* *< type *)
    passive: amqp_boolean_t; (* *< passive *)
    durable: amqp_boolean_t; (* *< durable *)
    auto_delete: amqp_boolean_t; (* *< auto-delete *)
    internal: amqp_boolean_t; (* *< internal *)
    nowait: amqp_boolean_t; (* *< nowait *)
    arguments: amqp_table_t; (* *< arguments *)
  end;

  amqp_exchange_declare_t = amqp_exchange_declare_t_;
  pamqp_exchange_declare_t = ^amqp_exchange_declare_t;

const
  AMQP_EXCHANGE_DECLARE_OK_METHOD: amqp_method_number_t = $0028000B; (* *< exchange.declare-ok method id @internal 40, 11; 2621451 *)

  (* * exchange.declare-ok method fields *)
type
  amqp_exchange_declare_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_exchange_declare_ok_t = amqp_exchange_declare_ok_t_;
  pamqp_exchange_declare_ok_t = ^amqp_exchange_declare_ok_t;

const
  AMQP_EXCHANGE_DELETE_METHOD: amqp_method_number_t = $00280014; (* *< exchange.delete method id @internal 40, 20; 2621460 *)

  (* * exchange.delete method fields *)
type
  amqp_exchange_delete_t_ = record
    ticket: UInt16; (* *< ticket *)
    exchange: amqp_bytes_t; (* *< exchange *)
    if_unused: amqp_boolean_t; (* *< if-unused *)
    nowait: amqp_boolean_t; (* *< nowait *)
  end;

  amqp_exchange_delete_t = amqp_exchange_delete_t_;
  pamqp_exchange_delete_t = ^amqp_exchange_delete_t;

const
  AMQP_EXCHANGE_DELETE_OK_METHOD: amqp_method_number_t = $00280015; (* *< exchange.delete-ok method id @internal 40, 21; 2621461 *)

  (* * exchange.delete-ok method fields *)
type
  amqp_exchange_delete_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_exchange_delete_ok_t = amqp_exchange_delete_ok_t_;
  pamqp_exchange_delete_ok_t = ^amqp_exchange_delete_ok_t;

const
  AMQP_EXCHANGE_BIND_METHOD: amqp_method_number_t = $0028001E; (* *< exchange.bind method id @internal 40, 30; 2621470 *)

  (* * exchange.bind method fields *)
type
  amqp_exchange_bind_t_ = record
    ticket: UInt16; (* *< ticket *)
    destination: amqp_bytes_t; (* *< destination *)
    source: amqp_bytes_t; (* *< source *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
    nowait: amqp_boolean_t; (* *< nowait *)
    arguments: amqp_table_t; (* *< arguments *)
  end;

  amqp_exchange_bind_t = amqp_exchange_bind_t_;
  pamqp_exchange_bind_t = ^amqp_exchange_bind_t;

const
  AMQP_EXCHANGE_BIND_OK_METHOD: amqp_method_number_t = $0028001F; (* *< exchange.bind-ok method id @internal 40, 31; 2621471 *)

  (* * exchange.bind-ok method fields *)
type
  amqp_exchange_bind_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_exchange_bind_ok_t = amqp_exchange_bind_ok_t_;
  pamqp_exchange_bind_ok_t = ^amqp_exchange_bind_ok_t;

const
  AMQP_EXCHANGE_UNBIND_METHOD: amqp_method_number_t = $00280028; (* *< exchange.unbind method id @internal 40, 40; 2621480 *)

  (* * exchange.unbind method fields *)
type
  amqp_exchange_unbind_t_ = record
    ticket: UInt16; (* *< ticket *)
    destination: amqp_bytes_t; (* *< destination *)
    source: amqp_bytes_t; (* *< source *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
    nowait: amqp_boolean_t; (* *< nowait *)
    arguments: amqp_table_t; (* *< arguments *)
  end;

  amqp_exchange_unbind_t = amqp_exchange_unbind_t_;
  pamqp_exchange_unbind_t = ^amqp_exchange_unbind_t;

const
  AMQP_EXCHANGE_UNBIND_OK_METHOD: amqp_method_number_t = $00280033; (* *< exchange.unbind-ok method id @internal 40, 51; 2621491 *)

  (* * exchange.unbind-ok method fields *)
type
  amqp_exchange_unbind_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_exchange_unbind_ok_t = amqp_exchange_unbind_ok_t_;
  pamqp_exchange_unbind_ok_t = ^amqp_exchange_unbind_ok_t;

const
  AMQP_QUEUE_DECLARE_METHOD: amqp_method_number_t = $0032000A; (* *< queue.declare method id @internal 50, 10; 3276810 *)

  (* * queue.declare method fields *)
type
  amqp_queue_declare_t_ = record
    ticket: UInt16; (* *< ticket *)
    queue: amqp_bytes_t; (* *< queue *)
    passive: amqp_boolean_t; (* *< passive *)
    durable: amqp_boolean_t; (* *< durable *)
    exclusive: amqp_boolean_t; (* *< exclusive *)
    auto_delete: amqp_boolean_t; (* *< auto-delete *)
    nowait: amqp_boolean_t; (* *< nowait *)
    arguments: amqp_table_t; (* *< arguments *)
  end;

  amqp_queue_declare_t = amqp_queue_declare_t_;
  pamqp_queue_declare_t = ^amqp_queue_declare_t;

const
  AMQP_QUEUE_DECLARE_OK_METHOD: amqp_method_number_t = $0032000B; (* *< queue.declare-ok method id @internal 50, 11; 3276811 *)

  (* * queue.declare-ok method fields *)
type
  amqp_queue_declare_ok_t_ = record
    queue: amqp_bytes_t; (* *< queue *)
    message_count: UInt32; (* *< message-count *)
    consumer_count: UInt32; (* *< consumer-count *)
  end;

  amqp_queue_declare_ok_t = amqp_queue_declare_ok_t_;
  pamqp_queue_declare_ok_t = ^amqp_queue_declare_ok_t;

const
  AMQP_QUEUE_BIND_METHOD: amqp_method_number_t = $00320014; (* *< queue.bind method id @internal 50, 20; 3276820 *)

  (* * queue.bind method fields *)
type
  amqp_queue_bind_t_ = record
    ticket: UInt16; (* *< ticket *)
    queue: amqp_bytes_t; (* *< queue *)
    exchange: amqp_bytes_t; (* *< exchange *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
    nowait: amqp_boolean_t; (* *< nowait *)
    arguments: amqp_table_t; (* *< arguments *)
  end;

  amqp_queue_bind_t = amqp_queue_bind_t_;
  pamqp_queue_bind_t = ^amqp_queue_bind_t;

const
  AMQP_QUEUE_BIND_OK_METHOD: amqp_method_number_t = $00320015; (* *< queue.bind-ok method id @internal 50, 21; 3276821 *)

  (* * queue.bind-ok method fields *)
type
  amqp_queue_bind_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_queue_bind_ok_t = amqp_queue_bind_ok_t_;
  pamqp_queue_bind_ok_t = ^amqp_queue_bind_ok_t;

const
  AMQP_QUEUE_PURGE_METHOD: amqp_method_number_t = $0032001E; (* *< queue.purge method id @internal 50, 30; 3276830 *)

  (* * queue.purge method fields *)
type
  amqp_queue_purge_t_ = record
    ticket: UInt16; (* *< ticket *)
    queue: amqp_bytes_t; (* *< queue *)
    nowait: amqp_boolean_t; (* *< nowait *)
  end;

  amqp_queue_purge_t = amqp_queue_purge_t_;
  pamqp_queue_purge_t = ^amqp_queue_purge_t;

const
  AMQP_QUEUE_PURGE_OK_METHOD: amqp_method_number_t = $0032001F; (* *< queue.purge-ok method id @internal 50, 31; 3276831 *)

  (* * queue.purge-ok method fields *)
type
  amqp_queue_purge_ok_t_ = record
    message_count: UInt32; (* *< message-count *)
  end;

  amqp_queue_purge_ok_t = amqp_queue_purge_ok_t_;
  pamqp_queue_purge_ok_t = ^amqp_queue_purge_ok_t;

const
  AMQP_QUEUE_DELETE_METHOD: amqp_method_number_t = $00320028; (* *< queue.delete method id @internal 50, 40; 3276840 *)

  (* * queue.delete method fields *)
type
  amqp_queue_delete_t_ = record
    ticket: UInt16; (* *< ticket *)
    queue: amqp_bytes_t; (* *< queue *)
    if_unused: amqp_boolean_t; (* *< if-unused *)
    if_empty: amqp_boolean_t; (* *< if-empty *)
    nowait: amqp_boolean_t; (* *< nowait *)
  end;

  amqp_queue_delete_t = amqp_queue_delete_t_;
  pamqp_queue_delete_t = ^amqp_queue_delete_t;

const
  AMQP_QUEUE_DELETE_OK_METHOD: amqp_method_number_t = $00320029; (* *< queue.delete-ok method id @internal 50, 41; 3276841 *)

  (* * queue.delete-ok method fields *)
type
  amqp_queue_delete_ok_t_ = record
    message_count: UInt32; (* *< message-count *)
  end;

  amqp_queue_delete_ok_t = amqp_queue_delete_ok_t_;
  pamqp_queue_delete_ok_t = ^amqp_queue_delete_ok_t;

const
  AMQP_QUEUE_UNBIND_METHOD: amqp_method_number_t = $00320032; (* *< queue.unbind method id @internal 50, 50; 3276850 *)

  (* * queue.unbind method fields *)
type
  amqp_queue_unbind_t_ = record
    ticket: UInt16; (* *< ticket *)
    queue: amqp_bytes_t; (* *< queue *)
    exchange: amqp_bytes_t; (* *< exchange *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
    arguments: amqp_table_t; (* *< arguments *)
  end;

  amqp_queue_unbind_t = amqp_queue_unbind_t_;
  pamqp_queue_unbind_t = ^amqp_queue_unbind_t;

const
  AMQP_QUEUE_UNBIND_OK_METHOD: amqp_method_number_t = $00320033; (* *< queue.unbind-ok method id @internal 50, 51; 3276851 *)

  (* * queue.unbind-ok method fields *)
type
  amqp_queue_unbind_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_queue_unbind_ok_t = amqp_queue_unbind_ok_t_;
  pamqp_queue_unbind_ok_t = ^amqp_queue_unbind_ok_t;

const
  AMQP_BASIC_QOS_METHOD: amqp_method_number_t = $003C000A; (* *< basic.qos method id @internal 60, 10; 3932170 *)

  (* * basic.qos method fields *)
type
  amqp_basic_qos_t_ = record
    prefetch_size: UInt32; (* *< prefetch-size *)
    prefetch_count: UInt16; (* *< prefetch-count *)
    global: amqp_boolean_t; (* *< global *)
  end;

  amqp_basic_qos_t = amqp_basic_qos_t_;
  pamqp_basic_qos_t = ^amqp_basic_qos_t;

const
  AMQP_BASIC_QOS_OK_METHOD: amqp_method_number_t = $003C000B; (* *< basic.qos-ok method id @internal 60, 11; 3932171 *)

  (* * basic.qos-ok method fields *)
type
  amqp_basic_qos_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_basic_qos_ok_t = amqp_basic_qos_ok_t_;
  pamqp_basic_qos_ok_t = ^amqp_basic_qos_ok_t;

const
  AMQP_BASIC_CONSUME_METHOD: amqp_method_number_t = $003C0014; (* *< basic.consume method id @internal 60, 20; 3932180 *)

  (* * basic.consume method fields *)
type
  amqp_basic_consume_t_ = record
    ticket: UInt16; (* *< ticket *)
    queue: amqp_bytes_t; (* *< queue *)
    consumer_tag: amqp_bytes_t; (* *< consumer-tag *)
    no_local: amqp_boolean_t; (* *< no-local *)
    no_ack: amqp_boolean_t; (* *< no-ack *)
    exclusive: amqp_boolean_t; (* *< exclusive *)
    nowait: amqp_boolean_t; (* *< nowait *)
    arguments: amqp_table_t; (* *< arguments *)
  end;

  amqp_basic_consume_t = amqp_basic_consume_t_;
  pamqp_basic_consume_t = ^amqp_basic_consume_t;

const
  AMQP_BASIC_CONSUME_OK_METHOD: amqp_method_number_t = $003C0015; (* *< basic.consume-ok method id @internal 60, 21; 3932181 *)

  (* * basic.consume-ok method fields *)
type
  amqp_basic_consume_ok_t_ = record
    consumer_tag: amqp_bytes_t; (* *< consumer-tag *)
  end;

  amqp_basic_consume_ok_t = amqp_basic_consume_ok_t_;
  pamqp_basic_consume_ok_t = ^amqp_basic_consume_ok_t;

const
  AMQP_BASIC_CANCEL_METHOD: amqp_method_number_t = $003C001E; (* *< basic.cancel method id @internal 60, 30; 3932190 *)

  (* * basic.cancel method fields *)
type
  amqp_basic_cancel_t_ = record
    consumer_tag: amqp_bytes_t; (* *< consumer-tag *)
    nowait: amqp_boolean_t; (* *< nowait *)
  end;

  amqp_basic_cancel_t = amqp_basic_cancel_t_;
  pamqp_basic_cancel_t = ^amqp_basic_cancel_t;

const
  AMQP_BASIC_CANCEL_OK_METHOD: amqp_method_number_t = $003C001F; (* *< basic.cancel-ok method id @internal 60, 31; 3932191 *)

  (* * basic.cancel-ok method fields *)
type
  amqp_basic_cancel_ok_t_ = record
    consumer_tag: amqp_bytes_t; (* *< consumer-tag *)
  end;

  amqp_basic_cancel_ok_t = amqp_basic_cancel_ok_t_;
  pamqp_basic_cancel_ok_t = ^amqp_basic_cancel_ok_t;

const
  AMQP_BASIC_PUBLISH_METHOD: amqp_method_number_t = $003C0028; (* *< basic.publish method id @internal 60, 40; 3932200 *)

  (* * basic.publish method fields *)
type
  amqp_basic_publish_t_ = record
    ticket: UInt16; (* *< ticket *)
    exchange: amqp_bytes_t; (* *< exchange *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
    mandatory: amqp_boolean_t; (* *< mandatory *)
    immediate: amqp_boolean_t; (* *< immediate *)
  end;

  amqp_basic_publish_t = amqp_basic_publish_t_;
  pamqp_basic_publish_t = ^amqp_basic_publish_t;

const
  AMQP_BASIC_RETURN_METHOD: amqp_method_number_t = $003C0032; (* *< basic.return method id @internal 60, 50; 3932210 *)

  (* * basic.return method fields *)
type
  amqp_basic_return_t_ = record
    reply_code: UInt16; (* *< reply-code *)
    reply_text: amqp_bytes_t; (* *< reply-text *)
    exchange: amqp_bytes_t; (* *< exchange *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
  end;

  amqp_basic_return_t = amqp_basic_return_t_;
  pamqp_basic_return_t = ^amqp_basic_return_t;

const
  AMQP_BASIC_DELIVER_METHOD: amqp_method_number_t = $003C003C; (* *< basic.deliver method id @internal 60, 60; 3932220 *)

  (* * basic.deliver method fields *)
type
  amqp_basic_deliver_t_ = record
    consumer_tag: amqp_bytes_t; (* *< consumer-tag *)
    delivery_tag: UInt64; (* *< delivery-tag *)
    redelivered: amqp_boolean_t; (* *< redelivered *)
    exchange: amqp_bytes_t; (* *< exchange *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
  end;

  amqp_basic_deliver_t = amqp_basic_deliver_t_;
  pamqp_basic_deliver_t = ^amqp_basic_deliver_t;

const
  AMQP_BASIC_GET_METHOD: amqp_method_number_t = $003C0046; (* *< basic.get method id @internal 60, 70; 3932230 *)

  (* * basic.get method fields *)
type
  amqp_basic_get_t_ = record
    ticket: UInt16; (* *< ticket *)
    queue: amqp_bytes_t; (* *< queue *)
    no_ack: amqp_boolean_t; (* *< no-ack *)
  end;

  amqp_basic_get_t = amqp_basic_get_t_;
  pamqp_basic_get_t = ^amqp_basic_get_t;

const
  AMQP_BASIC_GET_OK_METHOD: amqp_method_number_t = $003C0047; (* *< basic.get-ok method id @internal 60, 71; 3932231 *)

  (* * basic.get-ok method fields *)
type
  amqp_basic_get_ok_t_ = record
    delivery_tag: UInt64; (* *< delivery-tag *)
    redelivered: amqp_boolean_t; (* *< redelivered *)
    exchange: amqp_bytes_t; (* *< exchange *)
    routing_key: amqp_bytes_t; (* *< routing-key *)
    message_count: UInt32; (* *< message-count *)
  end;

  amqp_basic_get_ok_t = amqp_basic_get_ok_t_;
  pamqp_basic_get_ok_t = ^amqp_basic_get_ok_t;

const
  AMQP_BASIC_GET_EMPTY_METHOD: amqp_method_number_t = $003C0048; (* *< basic.get-empty method id @internal 60, 72; 3932232 *)

  (* * basic.get-empty method fields *)
type
  amqp_basic_get_empty_t_ = record
    cluster_id: amqp_bytes_t; (* *< cluster-id *)
  end;

  amqp_basic_get_empty_t = amqp_basic_get_empty_t_;
  pamqp_basic_get_empty_t = ^amqp_basic_get_empty_t;

const
  AMQP_BASIC_ACK_METHOD: amqp_method_number_t = $003C0050; (* *< basic.ack method id @internal 60, 80; 3932240 *)

  (* * basic.ack method fields *)
type
  amqp_basic_ack_t_ = record
    delivery_tag: UInt64; (* *< delivery-tag *)
    multiple: amqp_boolean_t; (* *< multiple *)
  end;

  amqp_basic_ack_t = amqp_basic_ack_t_;
  pamqp_basic_ack_t = ^amqp_basic_ack_t;

const
  AMQP_BASIC_REJECT_METHOD: amqp_method_number_t = $003C005A; (* *< basic.reject method id @internal 60, 90; 3932250 *)

  (* * basic.reject method fields *)
type
  amqp_basic_reject_t_ = record
    delivery_tag: UInt64; (* *< delivery-tag *)
    requeue: amqp_boolean_t; (* *< requeue *)
  end;

  amqp_basic_reject_t = amqp_basic_reject_t_;
  pamqp_basic_reject_t = ^amqp_basic_reject_t;

const
  AMQP_BASIC_RECOVER_ASYNC_METHOD: amqp_method_number_t = $003C0064; (* *< basic.recover-async method id @internal 60, 100; 3932260 *)

  (* * basic.recover-async method fields *)
type
  amqp_basic_recover_async_t_ = record
    requeue: amqp_boolean_t; (* *< requeue *)
  end;

  amqp_basic_recover_async_t = amqp_basic_recover_async_t_;
  pamqp_basic_recover_async_t = ^amqp_basic_recover_async_t;

const
  AMQP_BASIC_RECOVER_METHOD: amqp_method_number_t = $003C006E; (* *< basic.recover method id @internal 60, 110; 3932270 *)

  (* * basic.recover method fields *)
type
  amqp_basic_recover_t_ = record
    requeue: amqp_boolean_t; (* *< requeue *)
  end;

  amqp_basic_recover_t = amqp_basic_recover_t_;
  pamqp_basic_recover_t = ^amqp_basic_recover_t;

const
  AMQP_BASIC_RECOVER_OK_METHOD: amqp_method_number_t = $003C006F; (* *< basic.recover-ok method id @internal 60, 111; 3932271 *)

  (* * basic.recover-ok method fields *)
type
  amqp_basic_recover_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_basic_recover_ok_t = amqp_basic_recover_ok_t_;
  pamqp_basic_recover_ok_t = ^amqp_basic_recover_ok_t;

const
  AMQP_BASIC_NACK_METHOD: amqp_method_number_t = $003C0078; (* *< basic.nack method id @internal 60, 120; 3932280 *)

  (* * basic.nack method fields *)
type
  amqp_basic_nack_t_ = record
    delivery_tag: UInt64; (* *< delivery-tag *)
    multiple: amqp_boolean_t; (* *< multiple *)
    requeue: amqp_boolean_t; (* *< requeue *)
  end;

  amqp_basic_nack_t = amqp_basic_nack_t_;
  pamqp_basic_nack_t = ^amqp_basic_nack_t;

const
  AMQP_TX_SELECT_METHOD: amqp_method_number_t = $005A000A; (* *< tx.select method id @internal 90, 10; 5898250 *)

  (* * tx.select method fields *)
type
  amqp_tx_select_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_tx_select_t = amqp_tx_select_t_;
  pamqp_tx_select_t = ^amqp_tx_select_t;

const
  AMQP_TX_SELECT_OK_METHOD: amqp_method_number_t = $005A000B; (* *< tx.select-ok method id @internal 90, 11; 5898251 *)

  (* * tx.select-ok method fields *)
type
  amqp_tx_select_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_tx_select_ok_t = amqp_tx_select_ok_t_;
  pamqp_tx_select_ok_t = ^amqp_tx_select_ok_t;

const
  AMQP_TX_COMMIT_METHOD: amqp_method_number_t = $005A0014; (* *< tx.commit method id @internal 90, 20; 5898260 *)

  (* * tx.commit method fields *)
type
  amqp_tx_commit_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_tx_commit_t = amqp_tx_commit_t_;
  pamqp_tx_commit_t = ^amqp_tx_commit_t;

const
  AMQP_TX_COMMIT_OK_METHOD: amqp_method_number_t = $005A0015; (* *< tx.commit-ok method id @internal 90, 21; 5898261 *)

  (* * tx.commit-ok method fields *)
type
  amqp_tx_commit_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_tx_commit_ok_t = amqp_tx_commit_ok_t_;
  pamqp_tx_commit_ok_t = ^amqp_tx_commit_ok_t;

const
  AMQP_TX_ROLLBACK_METHOD: amqp_method_number_t = $005A001E; (* *< tx.rollback method id @internal 90, 30; 5898270 *)

  (* * tx.rollback method fields *)
type
  amqp_tx_rollback_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_tx_rollback_t = amqp_tx_rollback_t_;
  pamqp_tx_rollback_t = ^amqp_tx_rollback_t;

const
  AMQP_TX_ROLLBACK_OK_METHOD: amqp_method_number_t = $005A001F; (* *< tx.rollback-ok method id @internal 90, 31; 5898271 *)

  (* * tx.rollback-ok method fields *)
type
  amqp_tx_rollback_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_tx_rollback_ok_t = amqp_tx_rollback_ok_t_;
  pamqp_tx_rollback_ok_t = ^amqp_tx_rollback_ok_t;

const
  AMQP_CONFIRM_SELECT_METHOD: amqp_method_number_t = $0055000A; (* *< confirm.select method id @internal 85, 10; 5570570 *)

  (* * confirm.select method fields *)
type
  amqp_confirm_select_t_ = record
    nowait: amqp_boolean_t; (* *< nowait *)
  end;

  amqp_confirm_select_t = amqp_confirm_select_t_;
  pamqp_confirm_select_t = ^amqp_confirm_select_t;

const
  AMQP_CONFIRM_SELECT_OK_METHOD: amqp_method_number_t = $0055000B; (* *< confirm.select-ok method id @internal 85, 11; 5570571 *)

  (* * confirm.select-ok method fields *)
type
  amqp_confirm_select_ok_t_ = record
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_confirm_select_ok_t = amqp_confirm_select_ok_t_;
  pamqp_confirm_select_ok_t = ^amqp_confirm_select_ok_t;

  (* Class property records. *)
const
  AMQP_CONNECTION_CLASS = ($000A); (* *< connection class id @internal 10 *)

  (* * connection class properties *)
type
  amqp_connection_properties_t_ = record
    _flags: amqp_flags_t; (* *< bit-mask of set fields *)
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_connection_properties_t = amqp_connection_properties_t_;
  pamqp_connection_properties_t = ^amqp_connection_properties_t;

const
  AMQP_CHANNEL_CLASS = ($0014); (* *< channel class id @internal 20 *)

  (* * channel class properties *)
type
  amqp_channel_properties_t_ = record
    _flags: amqp_flags_t; (* *< bit-mask of set fields *)
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_channel_properties_t = amqp_channel_properties_t_;
  pamqp_channel_properties_t = ^amqp_channel_properties_t;

const
  AMQP_ACCESS_CLASS = ($001E); (* *< access class id @internal 30 *)

  (* * access class properties *)
type
  amqp_access_properties_t_ = record
    _flags: amqp_flags_t; (* *< bit-mask of set fields *)
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_access_properties_t = amqp_access_properties_t_;
  pamqp_access_properties_t = ^amqp_access_properties_t;

const
  AMQP_EXCHANGE_CLASS = ($0028); (* *< exchange class id @internal 40 *)

  (* * exchange class properties *)
type
  amqp_exchange_properties_t_ = record
    _flags: amqp_flags_t; (* *< bit-mask of set fields *)
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_exchange_properties_t = amqp_exchange_properties_t_;
  pamqp_exchange_properties_t = ^amqp_exchange_properties_t;

const
  AMQP_QUEUE_CLASS = ($0032); (* *< queue class id @internal 50 *)

  (* * queue class properties *)
type
  amqp_queue_properties_t_ = record
    _flags: amqp_flags_t; (* *< bit-mask of set fields *)
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_queue_properties_t = amqp_queue_properties_t_;
  pamqp_queue_properties_t = ^amqp_queue_properties_t;

const
  AMQP_BASIC_CLASS = ($003C); (* *< basic class id @internal 60 *)
  AMQP_BASIC_CONTENT_TYPE_FLAG = (1 shl 15); (* *< basic.content-type property flag *)
  AMQP_BASIC_CONTENT_ENCODING_FLAG = (1 shl 14); (* *< basic.content-encoding property flag *)
  AMQP_BASIC_HEADERS_FLAG = (1 shl 13); (* *< basic.headers property flag *)
  AMQP_BASIC_DELIVERY_MODE_FLAG = (1 shl 12); (* *< basic.delivery-mode property flag *)
  AMQP_BASIC_PRIORITY_FLAG = (1 shl 11); (* *< basic.priority property flag *)
  AMQP_BASIC_CORRELATION_ID_FLAG = (1 shl 10); (* *< basic.correlation-id property flag *)
  AMQP_BASIC_REPLY_TO_FLAG = (1 shl 9); (* *< basic.reply-to property flag *)
  AMQP_BASIC_EXPIRATION_FLAG = (1 shl 8); (* *< basic.expiration property flag *)
  AMQP_BASIC_MESSAGE_ID_FLAG = (1 shl 7); (* *< basic.message-id property flag *)
  AMQP_BASIC_TIMESTAMP_FLAG = (1 shl 6); (* *< basic.timestamp property flag *)
  AMQP_BASIC_TYPE_FLAG = (1 shl 5); (* *< basic.type property flag *)
  AMQP_BASIC_USER_ID_FLAG = (1 shl 4); (* *< basic.user-id property flag *)
  AMQP_BASIC_APP_ID_FLAG = (1 shl 3); (* *< basic.app-id property flag *)
  AMQP_BASIC_CLUSTER_ID_FLAG = (1 shl 2); (* *< basic.cluster-id property flag *)

  (* * basic class properties *)
type
  amqp_basic_properties_t_ = record
    _flags: amqp_flags_t; (* *< bit-mask of set fields *)
    content_type: amqp_bytes_t; (* *< content-type *)
    content_encoding: amqp_bytes_t; (* *< content-encoding *)
    headers: amqp_table_t; (* *< headers *)
    delivery_mode: UInt8; (* *< delivery-mode *)
    priority: UInt8; (* *< priority *)
    correlation_id: amqp_bytes_t; (* *< correlation-id *)
    reply_to: amqp_bytes_t; (* *< reply-to *)
    expiration: amqp_bytes_t; (* *< expiration *)
    message_id: amqp_bytes_t; (* *< message-id *)
    timestamp: UInt64; (* *< timestamp *)

    &type: amqp_bytes_t; (* *< type *)
    user_id: amqp_bytes_t; (* *< user-id *)
    app_id: amqp_bytes_t; (* *< app-id *)
    cluster_id: amqp_bytes_t; (* *< cluster-id *)
  end;

  amqp_basic_properties_t = amqp_basic_properties_t_;
  pamqp_basic_properties_t = ^amqp_basic_properties_t;
  pamqp_basic_properties_t_ = ^amqp_basic_properties_t_;

const
  AMQP_TX_CLASS = ($005A); (* *< tx class id @internal 90 *)

  (* * tx class properties *)
type
  amqp_tx_properties_t_ = record
    _flags: amqp_flags_t; (* *< bit-mask of set fields *)
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_tx_properties_t = amqp_tx_properties_t_;

const
  AMQP_CONFIRM_CLASS = ($0055); (* *< confirm class id @internal 85 *)

  (* * confirm class properties *)
type
  pamqp_confirm_properties_t_ = ^amqp_confirm_properties_t_;
  pamqp_confirm_properties_t = pamqp_confirm_properties_t_;

  amqp_confirm_properties_t_ = record
    _flags: amqp_flags_t;
    (* *< bit-mask of set fields *)
    dummy: char; (* *< Dummy field to avoid empty struct *)
  end;

  amqp_confirm_properties_t = amqp_confirm_properties_t_;

  (* API functions for methods *)

  (* *
    * amqp_channel_open
    *
    * @param [in] state connection state
    * @param [in] channel the channel to do the RPC on
    * @returns amqp_channel_open_ok_t
  *)
function amqp_channel_open(state: amqp_connection_state_t; channel: amqp_channel_t): pamqp_channel_open_ok_t; cdecl;

(* *
  * amqp_channel_flow
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] active active
  * @returns amqp_channel_flow_ok_t
*)
function amqp_channel_flow(state: amqp_connection_state_t; channel: amqp_channel_t; active: amqp_boolean_t): pamqp_channel_flow_ok_t; cdecl;
(* *
  * amqp_exchange_declare
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] exchange exchange
  * @param [in] type type
  * @param [in] passive passive
  * @param [in] durable durable
  * @param [in] auto_delete auto_delete
  * @param [in] internal internal
  * @param [in] arguments arguments
  * @returns amqp_exchange_declare_ok_t
*)
function amqp_exchange_declare(state: amqp_connection_state_t; channel: amqp_channel_t; exchange: amqp_bytes_t; &type: amqp_bytes_t; passive: amqp_boolean_t; durable: amqp_boolean_t;
  auto_delete: amqp_boolean_t; internal: amqp_boolean_t; arguments: amqp_table_t): pamqp_exchange_declare_ok_t; cdecl;
(* *
  * amqp_exchange_delete
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] exchange exchange
  * @param [in] if_unused if_unused
  * @returns amqp_exchange_delete_ok_t
*)
function amqp_exchange_delete(state: amqp_connection_state_t; channel: amqp_channel_t; exchange: amqp_bytes_t; if_unused: amqp_boolean_t): pamqp_exchange_delete_ok_t; cdecl;
(* *
  * amqp_exchange_bind
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] destination destination
  * @param [in] source source
  * @param [in] routing_key routing_key
  * @param [in] arguments arguments
  * @returns amqp_exchange_bind_ok_t
*)
function amqp_exchange_bind(state: amqp_connection_state_t; channel: amqp_channel_t; destination: amqp_bytes_t; source: amqp_bytes_t; routing_key: amqp_bytes_t; arguments: amqp_table_t)
  : pamqp_exchange_bind_ok_t; cdecl;
(* *
  * amqp_exchange_unbind
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] destination destination
  * @param [in] source source
  * @param [in] routing_key routing_key
  * @param [in] arguments arguments
  * @returns amqp_exchange_unbind_ok_t
*)
function amqp_exchange_unbind(state: amqp_connection_state_t; channel: amqp_channel_t; destination: amqp_bytes_t; source: amqp_bytes_t; routing_key: amqp_bytes_t; arguments: amqp_table_t)
  : pamqp_exchange_unbind_ok_t; cdecl;
(* *
  * amqp_queue_declare
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] queue queue
  * @param [in] passive passive
  * @param [in] durable durable
  * @param [in] exclusive exclusive
  * @param [in] auto_delete auto_delete
  * @param [in] arguments arguments
  * @returns amqp_queue_declare_ok_t
*)
function amqp_queue_declare(state: amqp_connection_state_t; channel: amqp_channel_t; queue: amqp_bytes_t; passive: amqp_boolean_t; durable: amqp_boolean_t; exclusive: amqp_boolean_t;
  auto_delete: amqp_boolean_t; arguments: amqp_table_t): pamqp_exchange_unbind_ok_t; cdecl;
(* *
  * amqp_queue_bind
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] queue queue
  * @param [in] exchange exchange
  * @param [in] routing_key routing_key
  * @param [in] arguments arguments
  * @returns amqp_queue_bind_ok_t
*)
function amqp_queue_bind(state: amqp_connection_state_t; channel: amqp_channel_t; queue: amqp_bytes_t; exchange: amqp_bytes_t; routing_key: amqp_bytes_t; arguments: amqp_table_t)
  : pamqp_queue_bind_ok_t; cdecl;
(* *
  * amqp_queue_purge
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] queue queue
  * @returns amqp_queue_purge_ok_t
*)
function amqp_queue_purge(state: amqp_connection_state_t; channel: amqp_channel_t; queue: amqp_bytes_t): pamqp_queue_purge_ok_t; cdecl;
(* *
  * amqp_queue_delete
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] queue queue
  * @param [in] if_unused if_unused
  * @param [in] if_empty if_empty
  * @returns amqp_queue_delete_ok_t
*)
function amqp_queue_delete(state: amqp_connection_state_t; channel: amqp_channel_t; queue: amqp_bytes_t; if_unused: amqp_boolean_t; if_empty: amqp_boolean_t): pamqp_queue_delete_ok_t; cdecl;
(* *
  * amqp_queue_unbind
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] queue queue
  * @param [in] exchange exchange
  * @param [in] routing_key routing_key
  * @param [in] arguments arguments
  * @returns amqp_queue_unbind_ok_t
*)
function amqp_queue_unbind(state: amqp_connection_state_t; channel: amqp_channel_t; queue: amqp_bytes_t; exchange: amqp_bytes_t; routing_key: amqp_bytes_t; arguments: amqp_table_t)
  : pamqp_queue_unbind_ok_t; cdecl;
(* *
  * amqp_basic_qos
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] prefetch_size prefetch_size
  * @param [in] prefetch_count prefetch_count
  * @param [in] global global
  * @returns amqp_basic_qos_ok_t
*)
function amqp_basic_qos(state: amqp_connection_state_t; channel: amqp_channel_t; prefetch_size: UInt32; prefetch_count: UInt16; global: amqp_boolean_t): pamqp_basic_qos_ok_t; cdecl;
(* *
  * amqp_basic_consume
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] queue queue
  * @param [in] consumer_tag consumer_tag
  * @param [in] no_local no_local
  * @param [in] no_ack no_ack
  * @param [in] exclusive exclusive
  * @param [in] arguments arguments
  * @returns amqp_basic_consume_ok_t
*)
function amqp_basic_consume(state: amqp_connection_state_t; channel: amqp_channel_t; queue: amqp_bytes_t; consumer_tag: amqp_bytes_t; no_local: amqp_boolean_t; no_ack: amqp_boolean_t;
  exclusive: amqp_boolean_t; arguments: amqp_table_t): pamqp_basic_consume_ok_t; cdecl;
(* *
  * amqp_basic_cancel
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] consumer_tag consumer_tag
  * @returns amqp_basic_cancel_ok_t
*)
function amqp_basic_cancel(state: amqp_connection_state_t; channel: amqp_channel_t; consumer_tag: amqp_bytes_t): pamqp_basic_cancel_ok_t; cdecl;
(* *
  * amqp_basic_recover
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @param [in] requeue requeue
  * @returns amqp_basic_recover_ok_t
*)
function amqp_basic_recover(state: amqp_connection_state_t; channel: amqp_channel_t; requeue: amqp_boolean_t): pamqp_basic_recover_ok_t; cdecl;
(* *
  * amqp_tx_select
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @returns amqp_tx_select_ok_t
*)
function amqp_tx_select(state: amqp_connection_state_t; channel: amqp_channel_t): pamqp_tx_select_ok_t; cdecl;
(* *
  * amqp_tx_commit
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @returns amqp_tx_commit_ok_t
*)
function amqp_tx_commit(state: amqp_connection_state_t; channel: amqp_channel_t): pamqp_tx_commit_ok_t; cdecl;
(* *
  * amqp_tx_rollback
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @returns amqp_tx_rollback_ok_t
*)
function amqp_tx_rollback(state: amqp_connection_state_t; channel: amqp_channel_t): pamqp_tx_rollback_ok_t; cdecl;
(* *
  * amqp_confirm_select
  *
  * @param [in] state connection state
  * @param [in] channel the channel to do the RPC on
  * @returns amqp_confirm_select_ok_t
*)
function amqp_confirm_select(state: amqp_connection_state_t; channel: amqp_channel_t): pamqp_confirm_select_ok_t; cdecl;

{$ENDREGION}
{$REGION 'amqp.h'}

const
  AMQP_VERSION_MAJOR = 0;
  AMQP_VERSION_MINOR = 8;
  AMQP_VERSION_PATCH = 1;
  AMQP_VERSION_IS_RELEASE = 0;

  (* *
    * Returns the rabbitmq-c version as a packed integer.
    *
    * See \ref AMQP_VERSION
    *
    * \return packed 32-bit integer representing version of library at runtime
    *
    * \sa AMQP_VERSION, amqp_version()
    *
    * \since v0.4.0
  *)

function amqp_version_number: UInt32; cdecl;

(* *
  * Returns the rabbitmq-c version as a string.
  *
  * See \ref AMQP_VERSION_STRING
  *
  * \return a statically allocated string describing the version of rabbitmq-c.
  *
  * \sa amqp_version_number(), AMQP_VERSION_STRING, AMQP_VERSION
  *
  * \since v0.1
*)
function amqp_version: PAnsiChar; cdecl;

(* *
  * \def AMQP_DEFAULT_FRAME_SIZE
  *
  * Default frame size (128Kb)
  *
  * \sa amqp_login(), amqp_login_with_properties()
  *
  * \since v0.4.0
*)
const
  AMQP_DEFAULT_FRAME_SIZE = 131072;

  (* *
    * \def AMQP_DEFAULT_MAX_CHANNELS
    *
    * Default maximum number of channels (0, no limit)
    *
    * \sa amqp_login(), amqp_login_with_properties()
    *
    * \since v0.4.0
  *)
  AMQP_DEFAULT_MAX_CHANNELS = 0;

  (* *
    * \def AMQP_DEFAULT_HEARTBEAT
    *
    * Default heartbeat interval (0, heartbeat disabled)
    *
    * \sa amqp_login(), amqp_login_with_properties()
    *
    * \since v0.4.0
  *)
  AMQP_DEFAULT_HEARTBEAT = 0;

  (* *
    * boolean type 0 = false, true otherwise
    *
    * \since v0.1
  *)
type

  (*
    0-9   0-9-1   Qpid/Rabbit  Type               Remarks
    ---------------------------------------------------------------------------
    t       t            Boolean
    b       b            Signed 8-bit
    B                    Unsigned 8-bit
    U       s            Signed 16-bit      (A1)
    u                    Unsigned 16-bit
    I     I       I            Signed 32-bit
    i                    Unsigned 32-bit
    L       l            Signed 64-bit      (B)
    l                    Unsigned 64-bit
    f       f            32-bit float
    d       d            64-bit float
    D     D       D            Decimal
    s                    Short string       (A2)
    S     S       S            Long string
    A                    Nested Array
    T     T       T            Timestamp (u64)
    F     F       F            Nested Table
    V     V       V            Void
    x            Byte array

    Remarks:

    A1, A2: Notice how the types **CONFLICT** here. In Qpid and Rabbit,
    's' means a signed 16-bit integer; in 0-9-1, it means a
    short string.

    B: Notice how the signednesses **CONFLICT** here. In Qpid and Rabbit,
    'l' means a signed 64-bit integer; in 0-9-1, it means an unsigned
    64-bit integer.

    I'm going with the Qpid/Rabbit types, where there's a conflict, and
    the 0-9-1 types otherwise. 0-8 is a subset of 0-9, which is a subset
    of the other two, so this will work for both 0-8 and 0-9-1 branches of
    the code.
  *)

  (* *
    * Field value types
    *
    * \since v0.1
  *)
  amqp_field_value_kind_t = (AMQP_FIELD_KIND_BOOLEAN = Ord('t'), (* < boolean type. 0 = false, 1 = true @see amqp_boolean_t *)
    AMQP_FIELD_KIND_I8 = Ord('b'), (* < 8-bit signed integer, datatype: int8_t *)
    AMQP_FIELD_KIND_U8 = Ord('B'), (* < 8-bit unsigned integer, datatype: uint8_t *)
    AMQP_FIELD_KIND_I16 = Ord('s'), (* < 16-bit signed integer, datatype: int16_t *)
    AMQP_FIELD_KIND_U16 = Ord('u'), (* < 16-bit unsigned integer, datatype: uint16_t *)
    AMQP_FIELD_KIND_I32 = Ord('I'), (* < 32-bit signed integer, datatype: int32_t *)
    AMQP_FIELD_KIND_U32 = Ord('i'), (* < 32-bit unsigned integer, datatype: uint32_t *)
    AMQP_FIELD_KIND_I64 = Ord('l'), (* < 64-bit signed integer, datatype: int64_t *)
    AMQP_FIELD_KIND_U64 = Ord('L'), (* < 64-bit unsigned integer, datatype: UInt64 *)
    AMQP_FIELD_KIND_F32 = Ord('f'), (* < single-precision floating point value, datatype: float *)
    AMQP_FIELD_KIND_F64 = Ord('d'), (* < double-precision floating point value, datatype: double *)
    AMQP_FIELD_KIND_DECIMAL = Ord('D'), (* < amqp-decimal value, datatype: amqp_decimal_t *)
    AMQP_FIELD_KIND_UTF8 = Ord('S'), (* < UTF-8 null-terminated character string, datatype: amqp_bytes_t *)
    AMQP_FIELD_KIND_ARRAY = Ord('A'), (* < field array (repeated values of another datatype. datatype: amqp_array_t *)
    AMQP_FIELD_KIND_TIMESTAMP = Ord('T'), (* < 64-bit timestamp. datatype UInt64 *)
    AMQP_FIELD_KIND_TABLE = Ord('F'), (* < field table. encapsulates a table inside a table entry. datatype: amqp_table_t *)
    AMQP_FIELD_KIND_VOID = Ord('V'), (* < empty entry *)
    AMQP_FIELD_KIND_BYTES = Ord('x') (* < unformatted byte string, datatype: amqp_bytes_t *)
    );

  (* *
    * An AMQP frame
    *
    * \since v0.1
  *)
  pamqp_frame_t = ^amqp_frame_t_;

  amqp_frame_t_ = record
    frame_type: UInt8; (* *< frame type. The types:
      * - AMQP_FRAME_METHOD - use the method union member
      * - AMQP_FRAME_HEADER - use the properties union member
      * - AMQP_FRAME_BODY - use the body_fragment union member
    *)
    channel: amqp_channel_t; (* *< the channel the frame was received on *)

    payload: record
      case integer of
        0:
          (properties: record class_id: UInt16; (* < the class for the properties *)
            body_size: UInt64; (* < size of the body in bytes *)
            decoded: Pointer; (* < the decoded properties *)
            raw: amqp_bytes_t; (* < amqp-encoded properties structure *)
          end);
        1:
          (body_fragment: amqp_bytes_t); (* < a body fragment, use if frame_type == AMQP_FRAME_BODY *)
        2:
          (protocol_header: record transport_high: UInt8; (* < @internal first byte of handshake *)
            transport_low: UInt8; (* < @internal second byte of handshake *)
            protocol_version_major: UInt8; (* < @internal third byte of handshake *)
            protocol_version_minor: UInt8; (* < @internal fourth byte of handshake *)
          end); (* < Used only when doing the initial handshake with the broker,
        don't use otherwise *)

    end;
  end;

  amqp_frame_t = amqp_frame_t_;

  (* *
    * SASL method type
    *
    * \since v0.1
  *)
  amqp_sasl_method_enum_ = (AMQP_SASL_METHOD_UNDEFINED = -1, (* Invalid SASL method *)
    AMQP_SASL_METHOD_PLAIN = 0, (* the PLAIN SASL method for authentication to the broker *)
    AMQP_SASL_METHOD_EXTERNAL = 1 (* the EXTERNAL SASL method for authentication to the broker *)
    );
  amqp_sasl_method_enum = amqp_sasl_method_enum_;

  (* *
    * connection state object
    *
    * \since v0.1
  *)
  // amqp_connection_state_t = ^amqp_connection_state_t_;

  // (* *
  // * Socket object
  // *
  // * \since v0.4.0
  // *)
  // amqp_socket_t = amqp_socket_t_;

  (* *
    * Status codes
    *
    * \since v0.4.0
  *)
  (* NOTE: When updating this enum, update the strings in librabbitmq/amqp_api.c *)
  amqp_status_enum_ = (AMQP_STATUS_OK = $0, (* Operation successful *)
    AMQP_STATUS_NO_MEMORY = -$0001, (* Memory allocation failed *)
    AMQP_STATUS_BAD_AMQP_DATA = -$0002, (* Incorrect or corrupt data was received from the broker.This is a protocol error. *)
    AMQP_STATUS_UNKNOWN_CLASS = -$0003, (* an unknown AMQP class was received.This is a protocol error. *)
    AMQP_STATUS_UNKNOWN_METHOD = -$0004, (* an unknown AMQP method was received.This is a protocol error. *)
    AMQP_STATUS_HOSTNAME_RESOLUTION_FAILED = -$0005, (* Unable to resolve the * hostname *)
    AMQP_STATUS_INCOMPATIBLE_AMQP_VERSION = -$0006, (* the broker advertised an incompaible AMQP version *)
    AMQP_STATUS_CONNECTION_CLOSED = -$0007, (* the connection to the broker has been closed *)
    AMQP_STATUS_BAD_URL = -$0008, (* malformed AMQP URL *)
    AMQP_STATUS_SOCKET_ERROR = -$0009, (* a socket error occurred *)
    AMQP_STATUS_INVALID_PARAMETER = -$000A, (* an Invalid parameter was passed into the function *)
    AMQP_STATUS_TABLE_TOO_BIG = -$000B, (* the amqp_table_t object cannot be serialized because the output buffer is too small *)
    AMQP_STATUS_WRONG_METHOD = -$000C, (* the wrong method was received *)
    AMQP_STATUS_TIMEOUT = -$000D, (* Operation timed out *)
    AMQP_STATUS_TIMER_FAILURE = -$000E, (* the underlying system timer facility failed *)
    AMQP_STATUS_HEARTBEAT_TIMEOUT = -$000F, (* timed out waiting for heartbeat *)
    AMQP_STATUS_UNEXPECTED_STATE = -$0010, (* Unexpected protocol state *)
    AMQP_STATUS_SOCKET_CLOSED = -$0011, (* underlying socket is closed *)
    AMQP_STATUS_SOCKET_INUSE = -$0012, (* underlying socket is already open *)
    AMQP_STATUS_BROKER_UNSUPPORTED_SASL_METHOD = -$0013, (* broker does not support the requested SASL mechanism *)
    AMQP_STATUS_UNSUPPORTED = -$0014, (* parameter is unsupported in This version *)
    _AMQP_STATUS_NEXT_VALUE = -$0015, (* Internal value *)
    AMQP_STATUS_TCP_ERROR = -$0100, (* a generic TCP error occurred *)
    AMQP_STATUS_TCP_SOCKETLIB_INIT_ERROR = -$0101, (* an error occurred trying to initialize the socket library *)
    _AMQP_STATUS_TCP_NEXT_VALUE = -$0102, (* Internal value *)
    AMQP_STATUS_SSL_ERROR = -$0200, (* a generic SSL error occurred. *)
    AMQP_STATUS_SSL_HOSTNAME_VERIFY_FAILED = -$0201, (* SSL validation of hostname against peer certificate failed *)
    AMQP_STATUS_SSL_PEER_VERIFY_FAILED = -$0202, (* SSL validation of peer certificate failed. *)
    AMQP_STATUS_SSL_CONNECTION_FAILED = -$0203, (* SSL handshake failed. *)
    _AMQP_STATUS_SSL_NEXT_VALUE = -$0204 (* Internal value *)
    );

  amqp_status_enum = amqp_status_enum_;

  (* *
    * AMQP delivery modes.
    * Use these values for the #amqp_basic_properties_t::delivery_mode field.
    *
    * \since v0.5
  *)
  amqp_delivery_mode_enum = (AMQP_DELIVERY_NONPERSISTENT = 1, (* Non - persistent message *)
    AMQP_DELIVERY_PERSISTENT = 2 (* persistent message *)
    );

  // {$INCLUDE <amqp_framing.h>}
  // (* *
  // * Empty bytes structure
  // *
  // * \since v0.2
  // *)
  // const
  // amqp_empty_bytes:
  // AMQP_BEGIN_DECLS { AMQP_PUBLIC_VARIABLE }{ <= !!!4 unknown type }{ amqp_bytes_t }{ <= !!!4 unknown type }{ AMQP_END_DECLS }{ <= !!!4 unknown type };
  // (* *
  // * Empty table structure
  // *
  // * \since v0.2
  // *)
  // amqp_empty_table:
  // amqp_table_t { AMQP_PUBLIC_VARIABLE }{ <= !!!4 unknown type };
  // (* *
  // * Empty table array structure
  // *
  // * \since v0.2
  // *)
  // amqp_empty_array:
  // amqp_array_t { AMQP_PUBLIC_VARIABLE }{ <= !!!4 unknown type };
  // (* Compatibility macros for the above, to avoid the need to update
  // code written against earlier versions of librabbitmq. *)
  //
  // (* *
  // * \def AMQP_EMPTY_BYTES
  // *
  // * Deprecated, use \ref amqp_empty_bytes instead
  // *
  // * \deprecated use \ref amqp_empty_bytes instead
  // *
  // * \since v0.1
  // *)
  // amqp_empty_bytes = amqp_empty_bytes;
  //
  // (* *
  // * \def AMQP_EMPTY_TABLE
  // *
  // * Deprecated, use \ref amqp_empty_table instead
  // *
  // * \deprecated use \ref amqp_empty_table instead
  // *
  // * \since v0.1
  // *)
  // amqp_empty_table = amqp_empty_table;
  //
  // (* *
  // * \def AMQP_EMPTY_ARRAY
  // *
  // * Deprecated, use \ref amqp_empty_array instead
  // *
  // * \deprecated use \ref amqp_empty_array instead
  // *
  // * \since v0.1
  // *)
  // amqp_empty_array = amqp_empty_array;

  (* *
    * Initializes an amqp_pool_t memory allocation pool for use
    *
    * Readies an allocation pool for use. An amqp_pool_t
    * must be initialized before use
    *
    * \param [in] pool the amqp_pool_t structure to initialize.
    *              Calling this function on a pool a pool that has
    *              already been initialized will result in undefined
    *              behavior
    * \param [in] pagesize the unit size that the pool will allocate
    *              memory chunks in. Anything allocated against the pool
    *              with a requested size will be carved out of a block
    *              this size. Allocations larger than this will be
    *              allocated individually
    *
    * \sa recycle_amqp_pool(), empty_amqp_pool(), amqp_pool_alloc(),
    *     amqp_pool_alloc_bytes(), amqp_pool_t
    *
    * \since v0.1
  *)

procedure init_amqp_pool(pool: pamqp_pool_t; pagesize: size_t); cdecl;

(* *
  * Recycles an amqp_pool_t memory allocation pool
  *
  * Recycles the space allocate by the pool
  *
  * This invalidates all allocations made against the pool before this call is
  * made, any use of any allocations made before recycle_amqp_pool() is called
  * will result in undefined behavior.
  *
  * Note: this may or may not release memory, to force memory to be released
  * call empty_amqp_pool().
  *
  * \param [in] pool the amqp_pool_t to recycle
  *
  * \sa recycle_amqp_pool(), empty_amqp_pool(), amqp_pool_alloc(),
  *      amqp_pool_alloc_bytes()
  *
  * \since v0.1
  *
*)

procedure recycle_amqp_pool(pool: pamqp_pool_t); cdecl;

(* *
  * Empties an amqp memory pool
  *
  * Releases all memory associated with an allocation pool
  *
  * \param [in] pool the amqp_pool_t to empty
  *
  * \since v0.1
*)

procedure empty_amqp_pool(pool: pamqp_pool_t); cdecl;

(* *
  * Allocates a block of memory from an amqp_pool_t memory pool
  *
  * Memory will be aligned on a 8-byte boundary. If a 0-length allocation is
  * requested, a NULL pointer will be returned.
  *
  * \param [in] pool the allocation pool to allocate the memory from
  * \param [in] amount the size of the allocation in bytes.
  * \return a pointer to the memory block, or NULL if the allocation cannot
  *          be satisfied.
  *
  * \sa init_amqp_pool(), recycle_amqp_pool(), empty_amqp_pool(),
  *     amqp_pool_alloc_bytes()
  *
  * \since v0.1
*)
function amqp_pool_alloc(pool: pamqp_pool_t; amount: size_t): Pointer; cdecl;

(* *
  * Allocates a block of memory from an amqp_pool_t to an amqp_bytes_t
  *
  * Memory will be aligned on a 8-byte boundary. If a 0-length allocation is
  * requested, output.bytes = NULL.
  *
  * \param [in] pool the allocation pool to allocate the memory from
  * \param [in] amount the size of the allocation in bytes
  * \param [in] output the location to store the pointer. On success
  *              output.bytes will be set to the beginning of the buffer
  *              output.len will be set to amount
  *              On error output.bytes will be set to NULL and output.len
  *              set to 0
  *
  * \sa init_amqp_pool(), recycle_amqp_pool(), empty_amqp_pool(),
  *     amqp_pool_alloc()
  *
  * \since v0.1
*)

procedure amqp_pool_alloc_bytes(pool: pamqp_pool_t; amount: size_t; output: pamqp_bytes_t); cdecl;

(* *
  * Wraps a c string in an amqp_bytes_t
  *
  * Takes a string, calculates its length and creates an
  * amqp_bytes_t that points to it. The string is not duplicated.
  *
  * For a given input cstr, The amqp_bytes_t output.bytes is the
  * same as cstr, output.len is the length of the string not including
  * the \0 terminator
  *
  * This function uses strlen() internally so cstr must be properly
  * terminated
  *
  * \param [in] cstr the c string to wrap
  * \return an amqp_bytes_t that describes the string
  *
  * \since v0.1
*)

function amqp_cstring_bytes(const cstr: PAnsiChar): amqp_bytes_t; cdecl;

(* *
  * Duplicates an amqp_bytes_t buffer.
  *
  * The buffer is cloned and the contents copied.
  *
  * The memory associated with the output is allocated
  * with amqp_bytes_malloc() and should be freed with
  * amqp_bytes_free()
  *
  * \param [in] src
  * \return a clone of the src
  *
  * \sa amqp_bytes_free(), amqp_bytes_malloc()
  *
  * \since v0.1
*)

function amqp_bytes_malloc_dup(src: amqp_bytes_t): amqp_bytes_t; cdecl;

(* *
  * Allocates a amqp_bytes_t buffer
  *
  * Creates an amqp_bytes_t buffer of the specified amount, the buffer should be
  * freed using amqp_bytes_free()
  *
  * \param [in] amount the size of the buffer in bytes
  * \returns an amqp_bytes_t with amount bytes allocated.
  *           output.bytes will be set to NULL on error
  *
  * \sa amqp_bytes_free(), amqp_bytes_malloc_dup()
  *
  * \since v0.1
*)

function amqp_bytes_malloc(amount: size_t): amqp_bytes_t; cdecl;

(* *
  * Frees an amqp_bytes_t buffer
  *
  * Frees a buffer allocated with amqp_bytes_malloc() or amqp_bytes_malloc_dup()
  *
  * Calling amqp_bytes_free on buffers not allocated with one
  * of those two functions will result in undefined behavior
  *
  * \param [in] bytes the buffer to free
  *
  * \sa amqp_bytes_malloc(), amqp_bytes_malloc_dup()
  *
  * \since v0.1
*)

procedure amqp_bytes_free(bytes: amqp_bytes_t); cdecl;

(* *
  * Allocate and initialize a new amqp_connection_state_t object
  *
  * amqp_connection_state_t objects created with this function
  * should be freed with amqp_destroy_connection()
  *
  * \returns an opaque pointer on success, NULL or 0 on failure.
  *
  * \sa amqp_destroy_connection()
  *
  * \since v0.1
*)

function amqp_new_connection: amqp_connection_state_t; cdecl;

(* *
  * Get the underlying socket descriptor for the connection
  *
  * \warning Use the socket returned from this function carefully, incorrect use
  * of the socket outside of the library will lead to undefined behavior.
  * Additionally rabbitmq-c may use the socket differently version-to-version,
  * what may work in one version, may break in the next version. Be sure to
  * throughly test any applications that use the socket returned by this
  * function especially when using a newer version of rabbitmq-c
  *
  * \param [in] state the connection object
  * \returns the socket descriptor if one has been set, -1 otherwise
  *
  * \sa amqp_tcp_socket_new(), amqp_ssl_socket_new(), amqp_socket_open()
  *
  * \since v0.1
*)

function amqp_get_sockfd(state: amqp_connection_state_t): integer; cdecl;

(* *
  * Deprecated, use amqp_tcp_socket_new() or amqp_ssl_socket_new()
  *
  * \deprecated Use amqp_tcp_socket_new() or amqp_ssl_socket_new()
  *
  * Sets the socket descriptor associated with the connection. The socket
  * should be connected to a broker, and should not be read to or written from
  * before calling this function.  A socket descriptor can be created and opened
  * using amqp_open_socket()
  *
  * \param [in] state the connection object
  * \param [in] sockfd the socket
  *
  * \sa amqp_open_socket(), amqp_tcp_socket_new(), amqp_ssl_socket_new()
  *
  * \since v0.1
*)
procedure amqp_set_sockfd(state: amqp_connection_state_t; sockfd: integer); cdecl;

(* *
  * Tune client side parameters
  *
  * \warning This function may call abort() if the connection is in a certain
  *  state. As such it should probably not be called code outside the library.
  *  connection parameters should be specified when calling amqp_login() or
  *  amqp_login_with_properties()
  *
  * This function changes channel_max, frame_max, and heartbeat parameters, on
  * the client side only. It does not try to renegotiate these parameters with
  * the broker. Using this function will lead to unexpected results.
  *
  * \param [in] state the connection object
  * \param [in] channel_max the maximum number of channels.
  *              The largest this can be is 65535
  * \param [in] frame_max the maximum size of an frame.
  *              The smallest this can be is 4096
  *              The largest this can be is 2147483647
  *              Unless you know what you're doing the recommended
  *              size is 131072 or 128KB
  * \param [in] heartbeat the number of seconds between heartbeats
  *
  * \return AMQP_STATUS_OK on success, an amqp_status_enum value otherwise.
  *  Possible error codes include:
  *  - AMQP_STATUS_NO_MEMORY memory allocation failed.
  *  - AMQP_STATUS_TIMER_FAILURE the underlying system timer indicated it
  *    failed.
  *
  * \sa amqp_login(), amqp_login_with_properties()
  *
  * \since v0.1
*)

function amqp_tune_connection(state: amqp_connection_state_t; channel_max: integer; frame_max: integer; heartbeat: integer): integer; cdecl;

(* *
  * Get the maximum number of channels the connection can handle
  *
  * The maximum number of channels is set when connection negotiation takes
  * place in amqp_login() or amqp_login_with_properties().
  *
  * \param [in] state the connection object
  * \return the maximum number of channels. 0 if there is no limit
  *
  * \since v0.1
*)

function amqp_get_channel_max(state: amqp_connection_state_t): integer; cdecl;

(* *
  * Get the maximum size of an frame the connection can handle
  *
  * The maximum size of an frame is set when connection negotiation takes
  * place in amqp_login() or amqp_login_with_properties().
  *
  * \param [in] state the connection object
  * \return the maximum size of an frame.
  *
  * \since v0.6
*)

function amqp_get_frame_max(state: amqp_connection_state_t): integer; cdecl;

(* *
  * Get the number of seconds between heartbeats of the connection
  *
  * The number of seconds between heartbeats is set when connection
  * negotiation takes place in amqp_login() or amqp_login_with_properties().
  *
  * \param [in] state the connection object
  * \return the number of seconds between heartbeats.
  *
  * \since v0.6
*)

function amqp_get_heartbeat(state: amqp_connection_state_t): integer; cdecl;

(* *
  * Destroys an amqp_connection_state_t object
  *
  * Destroys a amqp_connection_state_t object that was created with
  * amqp_new_connection(). If the connection with the broker is open, it will be
  * implicitly closed with a reply code of 200 (success). Any memory that
  * would be freed with amqp_maybe_release_buffers() or
  * amqp_maybe_release_buffers_on_channel() will be freed, and use of that
  * memory will caused undefined behavior.
  *
  * \param [in] state the connection object
  * \return AMQP_STATUS_OK on success. amqp_status_enum value failure
  *
  * \sa amqp_new_connection()
  *
  * \since v0.1
*)

function amqp_destroy_connection(state: amqp_connection_state_t): integer; cdecl;

(* *
  * Process incoming data
  *
  * \warning This is a low-level function intended for those who want to
  *  have greater control over input and output over the socket from the
  *  broker. Correctly using this function requires in-depth knowledge of AMQP
  *  and rabbitmq-c.
  *
  * For a given buffer of data received from the broker, decode the first
  * frame in the buffer. If more than one frame is contained in the input buffer
  * the return value will be less than the received_data size, the caller should
  * adjust received_data buffer descriptor to point to the beginning of the
  * buffer + the return value.
  *
  * \param [in] state the connection object
  * \param [in] received_data a buffer of data received from the broker. The
  *  function will return the number of bytes of the buffer it used. The
  *  function copies these bytes to an internal buffer: this part of the buffer
  *  may be reused after this function successfully completes.
  * \param [in,out] decoded_frame caller should pass in a pointer to an
  *  amqp_frame_t struct. If there is enough data in received_data for a
  *  complete frame, decoded_frame->frame_type will be set to something OTHER
  *  than 0. decoded_frame may contain members pointing to memory owned by
  *  the state object. This memory can be recycled with amqp_maybe_release_buffers()
  *  or amqp_maybe_release_buffers_on_channel()
  * \return number of bytes consumed from received_data or 0 if a 0-length
  *  buffer was passed. A negative return value indicates failure. Possible errors:
  *  - AMQP_STATUS_NO_MEMORY failure in allocating memory. The library is likely in
  *    an indeterminate state making recovery unlikely. Client should note the error
  *    and terminate the application
  *  - AMQP_STATUS_BAD_AMQP_DATA bad AMQP data was received. The connection
  *    should be shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_METHOD: an unknown method was received from the
  *    broker. This is likely a protocol error and the connection should be
  *    shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_CLASS: a properties frame with an unknown class
  *    was received from the broker. This is likely a protocol error and the
  *    connection should be shutdown immediately
  *
  * \since v0.1
*)

function amqp_handle_input(state: amqp_connection_state_t; received_data: amqp_bytes_t; decoded_frame: pamqp_frame_t): integer; cdecl;

(* *
  * Check to see if connection memory can be released
  *
  * \deprecated This function is deprecated in favor of
  *  amqp_maybe_release_buffers() or amqp_maybe_release_buffers_on_channel()
  *
  * Checks the state of an amqp_connection_state_t object to see if
  * amqp_release_buffers() can be called successfully.
  *
  * \param [in] state the connection object
  * \returns TRUE if the buffers can be released FALSE otherwise
  *
  * \sa amqp_release_buffers() amqp_maybe_release_buffers()
  *  amqp_maybe_release_buffers_on_channel()
  *
  * \since v0.1
*)

function amqp_release_buffers_ok(state: amqp_connection_state_t): amqp_boolean_t; cdecl;

(* *
  * Release amqp_connection_state_t owned memory
  *
  * \deprecated This function is deprecated in favor of
  *  amqp_maybe_release_buffers() or amqp_maybe_release_buffers_on_channel()
  *
  * \warning caller should ensure amqp_release_buffers_ok() returns true before
  *  calling this function. Failure to do so may result in abort() being called.
  *
  * Release memory owned by the amqp_connection_state_t for reuse by the
  * library. Use of any memory returned by the library before this function is
  * called will result in undefined behavior.
  *
  * \note internally rabbitmq-c tries to reuse memory when possible. As a result
  * its possible calling this function may not have a noticeable effect on
  * memory usage.
  *
  * \param [in] state the connection object
  *
  * \sa amqp_release_buffers_ok() amqp_maybe_release_buffers()
  *  amqp_maybe_release_buffers_on_channel()
  *
  * \since v0.1
*)

procedure amqp_release_buffers(state: amqp_connection_state_t); cdecl;

(* *
  * Release amqp_connection_state_t owned memory
  *
  * Release memory owned by the amqp_connection_state_t object related to any
  * channel, allowing reuse by the library. Use of any memory returned by the
  * library before this function is called with result in undefined behavior.
  *
  * \note internally rabbitmq-c tries to reuse memory when possible. As a result
  * its possible calling this function may not have a noticeable effect on
  * memory usage.
  *
  * \param [in] state the connection object
  *
  * \sa amqp_maybe_release_buffers_on_channel()
  *
  * \since v0.1
*)

procedure amqp_maybe_release_buffers(state: amqp_connection_state_t); cdecl;

(* *
  * Release amqp_connection_state_t owned memory related to a channel
  *
  * Release memory owned by the amqp_connection_state_t object related to the
  * specified channel, allowing reuse by the library. Use of any memory returned
  * the library for a specific channel will result in undefined behavior.
  *
  * \note internally rabbitmq-c tries to reuse memory when possible. As a result
  * its possible calling this function may not have a noticeable effect on
  * memory usage.
  *
  * \param [in] state the connection object
  * \param [in] channel the channel specifier for which memory should be
  *  released. Note that the library does not care about the state of the
  *  channel when calling this function
  *
  * \sa amqp_maybe_release_buffers()
  *
  * \since v0.4.0
*)

procedure amqp_maybe_release_buffers_on_channel(state: amqp_connection_state_t; channel: amqp_channel_t); cdecl;

(* *
  * Send a frame to the broker
  *
  * \param [in] state the connection object
  * \param [in] frame the frame to send to the broker
  * \return AMQP_STATUS_OK on success, an amqp_status_enum value on error.
  *  Possible error codes:
  *  - AMQP_STATUS_BAD_AMQP_DATA the serialized form of the method or
  *    properties was too large to fit in a single AMQP frame, or the
  *    method contains an invalid value. The frame was not sent.
  *  - AMQP_STATUS_TABLE_TOO_BIG the serialized form of an amqp_table_t is
  *    too large to fit in a single AMQP frame. Frame was not sent.
  *  - AMQP_STATUS_UNKNOWN_METHOD an invalid method type was passed in
  *  - AMQP_STATUS_UNKNOWN_CLASS an invalid properties type was passed in
  *  - AMQP_STATUS_TIMER_FAILURE system timer indicated failure. The frame
  *    was sent
  *  - AMQP_STATUS_SOCKET_ERROR
  *  - AMQP_STATUS_SSL_ERROR
  *
  * \since v0.1
*)

function amqp_send_frame(state: amqp_connection_state_t; frame: pamqp_frame_t): integer; cdecl;

(* *
  * Compare two table entries
  *
  * Works just like strcmp(), comparing two the table keys, datatype, then values
  *
  * \param [in] entry1 the entry on the left
  * \param [in] entry2 the entry on the right
  * \return 0 if entries are equal, 0 < if left is greater, 0 > if right is greater
  *
  * \since v0.1
*)

function amqp_table_entry_cmp(entry1: pinteger; entry2: pinteger): integer; cdecl;

(* *
  * Open a socket to a remote host
  *
  * \deprecated This function is deprecated in favor of amqp_socket_open()
  *
  * Looks up the hostname, then attempts to open a socket to the host using
  * the specified portnumber. It also sets various options on the socket to
  * improve performance and correctness.
  *
  * \param [in] hostname this can be a hostname or IP address.
  *              Both IPv4 and IPv6 are acceptable
  * \param [in] portnumber the port to connect on. RabbitMQ brokers
  *              listen on port 5672, and 5671 for SSL
  * \return a positive value indicates success and is the sockfd. A negative
  *  value (see amqp_status_enum)is returned on failure. Possible error codes:
  *  - AMQP_STATUS_TCP_SOCKETLIB_INIT_ERROR Initialization of underlying socket
  *    library failed.
  *  - AMQP_STATUS_HOSTNAME_RESOLUTION_FAILED hostname lookup failed.
  *  - AMQP_STATUS_SOCKET_ERROR a socket error occurred. errno or WSAGetLastError()
  *    may return more useful information.
  *
  * \note IPv6 support was added in v0.3
  *
  * \sa amqp_socket_open() amqp_set_sockfd()
  *
  * \since v0.1
*)

function amqp_open_socket(hostname: PAnsiChar; portnumber: integer): integer; cdecl;

(* *
  * Send initial AMQP header to the broker
  *
  * \warning this is a low level function intended for those who want to
  * interact with the broker at a very low level. Use of this function without
  * understanding what it does will result in AMQP protocol errors.
  *
  * This function sends the AMQP protocol header to the broker.
  *
  * \param [in] state the connection object
  * \return AMQP_STATUS_OK on success, a negative value on failure. Possible
  *  error codes:
  * - AMQP_STATUS_CONNECTION_CLOSED the connection to the broker was closed.
  * - AMQP_STATUS_SOCKET_ERROR a socket error occurred. It is likely the
  *   underlying socket has been closed. errno or WSAGetLastError() may provide
  *   further information.
  * - AMQP_STATUS_SSL_ERROR a SSL error occurred. The connection to the broker
  *   was closed.
  *
  * \since v0.1
*)

function amqp_send_header(state: amqp_connection_state_t): integer; cdecl;

(* *
  * Checks to see if there are any incoming frames ready to be read
  *
  * Checks to see if there are any amqp_frame_t objects buffered by the
  * amqp_connection_state_t object. Having one or more frames buffered means
  * that amqp_simple_wait_frame() or amqp_simple_wait_frame_noblock() will
  * return a frame without potentially blocking on a read() call.
  *
  * \param [in] state the connection object
  * \return TRUE if there are frames enqueued, FALSE otherwise
  *
  * \sa amqp_simple_wait_frame() amqp_simple_wait_frame_noblock()
  *  amqp_data_in_buffer()
  *
  * \since v0.1
*)

function amqp_frames_enqueued(state: amqp_connection_state_t): amqp_boolean_t; cdecl;

(* *
  * Read a single amqp_frame_t
  *
  * Waits for the next amqp_frame_t frame to be read from the broker.
  * This function has the potential to block for a long time in the case of
  * waiting for a basic.deliver method frame from the broker.
  *
  * The library may buffer frames. When an amqp_connection_state_t object
  * has frames buffered calling amqp_simple_wait_frame() will return an
  * amqp_frame_t without entering a blocking read(). You can test to see if
  * an amqp_connection_state_t object has frames buffered by calling the
  * amqp_frames_enqueued() function.
  *
  * The library has a socket read buffer. When there is data in an
  * amqp_connection_state_t read buffer, amqp_simple_wait_frame() may return an
  * amqp_frame_t without entering a blocking read(). You can test to see if an
  * amqp_connection_state_t object has data in its read buffer by calling the
  * amqp_data_in_buffer() function.
  *
  * \param [in] state the connection object
  * \param [out] decoded_frame the frame
  * \return AMQP_STATUS_OK on success, an amqp_status_enum value
  *  is returned otherwise. Possible errors include:
  *  - AMQP_STATUS_NO_MEMORY failure in allocating memory. The library is likely in
  *    an indeterminate state making recovery unlikely. Client should note the error
  *    and terminate the application
  *  - AMQP_STATUS_BAD_AMQP_DATA bad AMQP data was received. The connection
  *    should be shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_METHOD: an unknown method was received from the
  *    broker. This is likely a protocol error and the connection should be
  *    shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_CLASS: a properties frame with an unknown class
  *    was received from the broker. This is likely a protocol error and the
  *    connection should be shutdown immediately
  *  - AMQP_STATUS_HEARTBEAT_TIMEOUT timed out while waiting for heartbeat
  *    from the broker. The connection has been closed.
  *  - AMQP_STATUS_TIMER_FAILURE system timer indicated failure.
  *  - AMQP_STATUS_SOCKET_ERROR a socket error occurred. The connection has
  *    been closed
  *  - AMQP_STATUS_SSL_ERROR a SSL socket error occurred. The connection has
  *    been closed.
  *
  * \sa amqp_simple_wait_frame_noblock() amqp_frames_enqueued()
  *  amqp_data_in_buffer()
  *
  * \note as of v0.4.0 this function will no longer return heartbeat frames
  *  when enabled by specifying a non-zero heartbeat value in amqp_login().
  *  Heartbeating is handled internally by the library.
  *
  * \since v0.1
*)

function amqp_simple_wait_frame(state: amqp_connection_state_t; decoded_frame: pamqp_frame_t): integer; cdecl;

(* *
  * Read a single amqp_frame_t with a timeout.
  *
  * Waits for the next amqp_frame_t frame to be read from the broker, up to
  * a timespan specified by tv. The function will return AMQP_STATUS_TIMEOUT
  * if the timeout is reached. The tv value is not modified by the function.
  *
  * If a 0 timeval is specified, the function behaves as if its non-blocking: it
  * will test to see if a frame can be read from the broker, and return immediately.
  *
  * If NULL is passed in for tv, the function will behave like
  * amqp_simple_wait_frame() and block until a frame is received from the broker
  *
  * The library may buffer frames.  When an amqp_connection_state_t object
  * has frames buffered calling amqp_simple_wait_frame_noblock() will return an
  * amqp_frame_t without entering a blocking read(). You can test to see if an
  * amqp_connection_state_t object has frames buffered by calling the
  * amqp_frames_enqueued() function.
  *
  * The library has a socket read buffer. When there is data in an
  * amqp_connection_state_t read buffer, amqp_simple_wait_frame_noblock() may return
  * an amqp_frame_t without entering a blocking read(). You can test to see if an
  * amqp_connection_state_t object has data in its read buffer by calling the
  * amqp_data_in_buffer() function.
  *
  * \note This function does not return heartbeat frames. When enabled, heartbeating
  *  is handed internally internally by the library
  *
  * \param [in,out] state the connection object
  * \param [out] decoded_frame the frame
  * \param [in] tv the maximum time to wait for a frame to be read. Setting
  * tv->tv_sec = 0 and tv->tv_usec = 0 will do a non-blocking read. Specifying
  * NULL for tv will make the function block until a frame is read.
  * \return AMQP_STATUS_OK on success. An amqp_status_enum value is returned
  *  otherwise. Possible errors include:
  *  - AMQP_STATUS_TIMEOUT the timeout was reached while waiting for a frame
  *    from the broker.
  *  - AMQP_STATUS_INVALID_PARAMETER the tv parameter contains an invalid value.
  *  - AMQP_STATUS_NO_MEMORY failure in allocating memory. The library is likely in
  *    an indeterminate state making recovery unlikely. Client should note the error
  *    and terminate the application
  *  - AMQP_STATUS_BAD_AMQP_DATA bad AMQP data was received. The connection
  *    should be shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_METHOD: an unknown method was received from the
  *    broker. This is likely a protocol error and the connection should be
  *    shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_CLASS: a properties frame with an unknown class
  *    was received from the broker. This is likely a protocol error and the
  *    connection should be shutdown immediately
  *  - AMQP_STATUS_HEARTBEAT_TIMEOUT timed out while waiting for heartbeat
  *    from the broker. The connection has been closed.
  *  - AMQP_STATUS_TIMER_FAILURE system timer indicated failure.
  *  - AMQP_STATUS_SOCKET_ERROR a socket error occurred. The connection has
  *    been closed
  *  - AMQP_STATUS_SSL_ERROR a SSL socket error occurred. The connection has
  *    been closed.
  *
  * \sa amqp_simple_wait_frame() amqp_frames_enqueued() amqp_data_in_buffer()
  *
  * \since v0.4.0
*)

function amqp_simple_wait_frame_noblock(state: amqp_connection_state_t; decoded_frame: pamqp_frame_t; tv: ptimeval): integer; cdecl;

(* *
  * Waits for a specific method from the broker
  *
  * \warning You probably don't want to use this function. If this function
  *  doesn't receive exactly the frame requested it closes the whole connection.
  *
  * Waits for a single method on a channel from the broker.
  * If a frame is received that does not match expected_channel
  * or expected_method the program will abort
  *
  * \param [in] state the connection object
  * \param [in] expected_channel the channel that the method should be delivered on
  * \param [in] expected_method the method to wait for
  * \param [out] output the method
  * \returns AMQP_STATUS_OK on success. An amqp_status_enum value is returned
  *  otherwise. Possible errors include:
  *  - AMQP_STATUS_WRONG_METHOD a frame containing the wrong method, wrong frame
  *    type or wrong channel was received. The connection is closed.
  *  - AMQP_STATUS_NO_MEMORY failure in allocating memory. The library is likely in
  *    an indeterminate state making recovery unlikely. Client should note the error
  *    and terminate the application
  *  - AMQP_STATUS_BAD_AMQP_DATA bad AMQP data was received. The connection
  *    should be shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_METHOD: an unknown method was received from the
  *    broker. This is likely a protocol error and the connection should be
  *    shutdown immediately
  *  - AMQP_STATUS_UNKNOWN_CLASS: a properties frame with an unknown class
  *    was received from the broker. This is likely a protocol error and the
  *    connection should be shutdown immediately
  *  - AMQP_STATUS_HEARTBEAT_TIMEOUT timed out while waiting for heartbeat
  *    from the broker. The connection has been closed.
  *  - AMQP_STATUS_TIMER_FAILURE system timer indicated failure.
  *  - AMQP_STATUS_SOCKET_ERROR a socket error occurred. The connection has
  *    been closed
  *  - AMQP_STATUS_SSL_ERROR a SSL socket error occurred. The connection has
  *    been closed.
  *
  * \since v0.1
*)

function amqp_simple_wait_method(state: amqp_connection_state_t; expected_channel: amqp_channel_t; expected_method: amqp_method_number_t; output: pamqp_method_t): integer; cdecl;

(* *
  * Sends a method to the broker
  *
  * This is a thin wrapper around amqp_send_frame(), providing a way to send
  * a method to the broker on a specified channel.
  *
  * \param [in] state the connection object
  * \param [in] channel the channel object
  * \param [in] id the method number
  * \param [in] decoded the method object
  * \returns AMQP_STATUS_OK on success, an amqp_status_enum value otherwise.
  *  Possible errors include:
  *  - AMQP_STATUS_BAD_AMQP_DATA the serialized form of the method or
  *    properties was too large to fit in a single AMQP frame, or the
  *    method contains an invalid value. The frame was not sent.
  *  - AMQP_STATUS_TABLE_TOO_BIG the serialized form of an amqp_table_t is
  *    too large to fit in a single AMQP frame. Frame was not sent.
  *  - AMQP_STATUS_UNKNOWN_METHOD an invalid method type was passed in
  *  - AMQP_STATUS_UNKNOWN_CLASS an invalid properties type was passed in
  *  - AMQP_STATUS_TIMER_FAILURE system timer indicated failure. The frame
  *    was sent
  *  - AMQP_STATUS_SOCKET_ERROR
  *  - AMQP_STATUS_SSL_ERROR
  *
  * \since v0.1
*)

function amqp_send_method(state: amqp_connection_state_t; channel: amqp_channel_t; id: amqp_method_number_t; decoded: Pointer): integer; cdecl;

(* *
  * Sends a method to the broker and waits for a method response
  *
  * \param [in] state the connection object
  * \param [in] channel the channel object
  * \param [in] request_id the method number of the request
  * \param [in] expected_reply_ids a 0 terminated array of expected response
  *             method numbers
  * \param [in] decoded_request_method the method to be sent to the broker
  * \return a amqp_rpc_reply_t:
  *  - r.reply_type == AMQP_RESPONSE_NORMAL. RPC completed successfully
  *  - r.reply_type == AMQP_RESPONSE_SERVER_EXCEPTION. The broker returned an
  *    exception:
  *    - If r.reply.id == AMQP_CHANNEL_CLOSE_METHOD a channel exception
  *      occurred, cast r.reply.decoded to amqp_channel_close_t* to see details
  *      of the exception. The client should amqp_send_method() a
  *      amqp_channel_close_ok_t. The channel must be re-opened before it
  *      can be used again. Any resources associated with the channel
  *      (auto-delete exchanges, auto-delete queues, consumers) are invalid
  *      and must be recreated before attempting to use them again.
  *    - If r.reply.id == AMQP_CONNECTION_CLOSE_METHOD a connection exception
  *      occurred, cast r.reply.decoded to amqp_connection_close_t* to see
  *      details of the exception. The client amqp_send_method() a
  *      amqp_connection_close_ok_t and disconnect from the broker.
  *  - r.reply_type == AMQP_RESPONSE_LIBRARY_EXCEPTION. An exception occurred
  *    within the library. Examine r.library_error and compare it against
  *    amqp_status_enum values to determine the error.
  *
  * \sa amqp_simple_rpc_decoded()
  *
  * \since v0.1
*)

function amqp_simple_rpc(state: amqp_connection_state_t; channel: amqp_channel_t; request_id: amqp_method_number_t; expected_reply_ids: pamqp_method_number_t; decoded_request_method: Pointer)
  : amqp_rpc_reply_t; cdecl;

(* *
  * Sends a method to the broker and waits for a method response
  *
  * \param [in] state the connection object
  * \param [in] channel the channel object
  * \param [in] request_id the method number of the request
  * \param [in] reply_id the method number expected in response
  * \param [in] decoded_request_method the request method
  * \return a pointer to the method returned from the broker, or NULL on error.
  *  On error amqp_get_rpc_reply() will return an amqp_rpc_reply_t with
  *  details on the error that occurred.
  *
  * \since v0.1
*)
function amqp_simple_rpc_decoded(state: amqp_connection_state_t; channel: amqp_channel_t; request_id: amqp_method_number_t; reply_id: amqp_method_number_t; decoded_request_method: Pointer)
  : Pointer; cdecl;

(* *
  * Get the last global amqp_rpc_reply
  *
  * The API methods corresponding to most synchronous AMQP methods
  * return a pointer to the decoded method result.  Upon error, they
  * return NULL, and we need some way of discovering what, if anything,
  * went wrong. amqp_get_rpc_reply() returns the most recent
  * amqp_rpc_reply_t instance corresponding to such an API operation
  * for the given connection.
  *
  * Only use it for operations that do not themselves return
  * amqp_rpc_reply_t; operations that do return amqp_rpc_reply_t
  * generally do NOT update this per-connection-global amqp_rpc_reply_t
  * instance.
  *
  * \param [in] state the connection object
  * \return the most recent amqp_rpc_reply_t:
  *  - r.reply_type == AMQP_RESPONSE_NORMAL. RPC completed successfully
  *  - r.reply_type == AMQP_RESPONSE_SERVER_EXCEPTION. The broker returned an
  *    exception:
  *    - If r.reply.id == AMQP_CHANNEL_CLOSE_METHOD a channel exception
  *      occurred, cast r.reply.decoded to amqp_channel_close_t* to see details
  *      of the exception. The client should amqp_send_method() a
  *      amqp_channel_close_ok_t. The channel must be re-opened before it
  *      can be used again. Any resources associated with the channel
  *      (auto-delete exchanges, auto-delete queues, consumers) are invalid
  *      and must be recreated before attempting to use them again.
  *    - If r.reply.id == AMQP_CONNECTION_CLOSE_METHOD a connection exception
  *      occurred, cast r.reply.decoded to amqp_connection_close_t* to see
  *      details of the exception. The client amqp_send_method() a
  *      amqp_connection_close_ok_t and disconnect from the broker.
  *  - r.reply_type == AMQP_RESPONSE_LIBRARY_EXCEPTION. An exception occurred
  *    within the library. Examine r.library_error and compare it against
  *    amqp_status_enum values to determine the error.
  *
  * \sa amqp_simple_rpc_decoded()
  *
  * \since v0.1
*)

function amqp_get_rpc_reply(state: amqp_connection_state_t): amqp_rpc_reply_t; cdecl;

(* *
  * Login to the broker
  *
  * After using amqp_open_socket and amqp_set_sockfd, call
  * amqp_login to complete connecting to the broker
  *
  * \param [in] state the connection object
  * \param [in] vhost the virtual host to connect to on the broker. The default
  *              on most brokers is "/"
  * \param [in] channel_max the limit for number of channels for the connection.
  *              0 means no limit, and is a good default (AMQP_DEFAULT_MAX_CHANNELS)
  *              Note that the maximum number of channels the protocol supports
  *              is 65535 (2^16, with the 0-channel reserved). The server can
  *              set a lower channel_max and then the client will use the lowest
  *              of the two
  * \param [in] frame_max the maximum size of an AMQP frame on the wire to
  *              request of the broker for this connection. 4096 is the minimum
  *              size, 2^31-1 is the maximum, a good default is 131072 (128KB), or
  *              AMQP_DEFAULT_FRAME_SIZE
  * \param [in] heartbeat the number of seconds between heartbeat frames to
  *              request of the broker. A value of 0 disables heartbeats.
  *              Note rabbitmq-c only has partial support for heartbeats, as of
  *              v0.4.0 they are only serviced during amqp_basic_publish() and
  *              amqp_simple_wait_frame()/amqp_simple_wait_frame_noblock()
  * \param [in] sasl_method the SASL method to authenticate with the broker.
  *              followed by the authentication information.
  *              For AMQP_SASL_METHOD_PLAIN, the AMQP_SASL_METHOD_PLAIN
  *              should be followed by two arguments in this order:
  *              const char* username, and const char* password.
  * \return amqp_rpc_reply_t indicating success or failure.
  *  - r.reply_type == AMQP_RESPONSE_NORMAL. Login completed successfully
  *  - r.reply_type == AMQP_RESPONSE_LIBRARY_EXCEPTION. In most cases errors
  *    from the broker when logging in will be represented by the broker closing
  *    the socket. In this case r.library_error will be set to
  *    AMQP_STATUS_CONNECTION_CLOSED. This error can represent a number of
  *    error conditions including: invalid vhost, authentication failure.
  *  - r.reply_type == AMQP_RESPONSE_SERVER_EXCEPTION. The broker returned an
  *    exception:
  *    - If r.reply.id == AMQP_CHANNEL_CLOSE_METHOD a channel exception
  *      occurred, cast r.reply.decoded to amqp_channel_close_t* to see details
  *      of the exception. The client should amqp_send_method() a
  *      amqp_channel_close_ok_t. The channel must be re-opened before it
  *      can be used again. Any resources associated with the channel
  *      (auto-delete exchanges, auto-delete queues, consumers) are invalid
  *      and must be recreated before attempting to use them again.
  *    - If r.reply.id == AMQP_CONNECTION_CLOSE_METHOD a connection exception
  *      occurred, cast r.reply.decoded to amqp_connection_close_t* to see
  *      details of the exception. The client amqp_send_method() a
  *      amqp_connection_close_ok_t and disconnect from the broker.
  *
  * \since v0.1
*)

function amqp_login(state: amqp_connection_state_t; const vhost: PAnsiChar; channel_max: integer; frame_max: integer; heartbeat: integer; sasl_method: amqp_sasl_method_enum): amqp_rpc_reply_t;
  varargs; cdecl;

(* *
  * Login to the broker passing a properties table
  *
  * This function is similar to amqp_login() and differs in that it provides a
  * way to pass client properties to the broker. This is commonly used to
  * negotiate newer protocol features as they are supported by the broker.
  *
  * \param [in] state the connection object
  * \param [in] vhost the virtual host to connect to on the broker. The default
  *              on most brokers is "/"
  * \param [in] channel_max the limit for the number of channels for the connection.
  *             0 means no limit, and is a good default (AMQP_DEFAULT_MAX_CHANNELS)
  *             Note that the maximum number of channels the protocol supports
  *             is 65535 (2^16, with the 0-channel reserved). The server can
  *             set a lower channel_max and then the client will use the lowest
  *             of the two
  * \param [in] frame_max the maximum size of an AMQP frame ont he wire to
  *              request of the broker for this connection. 4096 is the minimum
  *              size, 2^31-1 is the maximum, a good default is 131072 (128KB), or
  *              AMQP_DEFAULT_FRAME_SIZE
  * \param [in] heartbeat the number of seconds between heartbeat frame to
  *             request of the broker. A value of 0 disables heartbeats.
  *             Note rabbitmq-c only has partial support for hearts, as of
  *             v0.4.0 heartbeats are only serviced during amqp_basic_publish(),
  *             and amqp_simple_wait_frame()/amqp_simple_wait_frame_noblock()
  * \param [in] properties a table of properties to send the broker.
  * \param [in] sasl_method the SASL method to authenticate with the broker
  *             followed by the authentication information.
  *             For AMQP_SASL_METHOD_PLAN, the AMQP_SASL_METHOD_PLAIN parameter
  *             should be followed by two arguments in this order:
  *             const char* username, and const char* password.
  * \return amqp_rpc_reply_t indicating success or failure.
  *  - r.reply_type == AMQP_RESPONSE_NORMAL. Login completed successfully
  *  - r.reply_type == AMQP_RESPONSE_LIBRARY_EXCEPTION. In most cases errors
  *    from the broker when logging in will be represented by the broker closing
  *    the socket. In this case r.library_error will be set to
  *    AMQP_STATUS_CONNECTION_CLOSED. This error can represent a number of
  *    error conditions including: invalid vhost, authentication failure.
  *  - r.reply_type == AMQP_RESPONSE_SERVER_EXCEPTION. The broker returned an
  *    exception:
  *    - If r.reply.id == AMQP_CHANNEL_CLOSE_METHOD a channel exception
  *      occurred, cast r.reply.decoded to amqp_channel_close_t* to see details
  *      of the exception. The client should amqp_send_method() a
  *      amqp_channel_close_ok_t. The channel must be re-opened before it
  *      can be used again. Any resources associated with the channel
  *      (auto-delete exchanges, auto-delete queues, consumers) are invalid
  *      and must be recreated before attempting to use them again.
  *    - If r.reply.id == AMQP_CONNECTION_CLOSE_METHOD a connection exception
  *      occurred, cast r.reply.decoded to amqp_connection_close_t* to see
  *      details of the exception. The client amqp_send_method() a
  *      amqp_connection_close_ok_t and disconnect from the broker.
  *
  * \since v0.4.0
*)

function amqp_login_with_properties(state: amqp_connection_state_t; const vhost: PAnsiChar; channel_max: integer; frame_max: integer; heartbeat: integer; const properties: pamqp_table_t;
  sasl_method: amqp_sasl_method_enum): amqp_rpc_reply_t; varargs; cdecl;

// type
// pamqp_basic_properties_t_ = ^amqp_basic_properties_t_;
//
// amqp_basic_properties_t_ ;
//
// amqp_basic_properties_t = amqp_basic_properties_t_;
(* *
  * Publish a message to the broker
  *
  * Publish a message on an exchange with a routing key.
  *
  * Note that at the AMQ protocol level basic.publish is an async method:
  * this means error conditions that occur on the broker (such as publishing to
  * a non-existent exchange) will not be reflected in the return value of this
  * function.
  *
  * \param [in] state the connection object
  * \param [in] channel the channel identifier
  * \param [in] exchange the exchange on the broker to publish to
  * \param [in] routing_key the routing key to use when publishing the message
  * \param [in] mandatory indicate to the broker that the message MUST be routed
  *              to a queue. If the broker cannot do this it should respond with
  *              a basic.return method.
  * \param [in] immediate indicate to the broker that the message MUST be delivered
  *              to a consumer immediately. If the broker cannot do this it should
  *              response with a basic.return method.
  * \param [in] properties the properties associated with the message
  * \param [in] body the message body
  * \return AMQP_STATUS_OK on success, amqp_status_enum value on failure. Note
  *         that basic.publish is an async method, the return value from this
  *         function only indicates that the message data was successfully
  *         transmitted to the broker. It does not indicate failures that occur
  *         on the broker, such as publishing to a non-existent exchange.
  *         Possible error values:
  *         - AMQP_STATUS_TIMER_FAILURE: system timer facility returned an error
  *           the message was not sent.
  *         - AMQP_STATUS_HEARTBEAT_TIMEOUT: connection timed out waiting for a
  *           heartbeat from the broker. The message was not sent.
  *         - AMQP_STATUS_NO_MEMORY: memory allocation failed. The message was
  *           not sent.
  *         - AMQP_STATUS_TABLE_TOO_BIG: a table in the properties was too large
  *           to fit in a single frame. Message was not sent.
  *         - AMQP_STATUS_CONNECTION_CLOSED: the connection was closed.
  *         - AMQP_STATUS_SSL_ERROR: a SSL error occurred.
  *         - AMQP_STATUS_TCP_ERROR: a TCP error occurred. errno or
  *           WSAGetLastError() may provide more information
  *
  * Note: this function does heartbeat processing as of v0.4.0
  *
  * \since v0.1
*)

function amqp_basic_publish(state: amqp_connection_state_t; channel: amqp_channel_t; exchange: amqp_bytes_t; routing_key: amqp_bytes_t; mandatory: amqp_boolean_t; immediate: amqp_boolean_t;
  const properties: pamqp_basic_properties_t_; body: amqp_bytes_t): integer; cdecl;

(* *
  * Closes an channel
  *
  * \param [in] state the connection object
  * \param [in] channel the channel identifier
  * \param [in] code the reason for closing the channel, AMQP_REPLY_SUCCESS is a good default
  * \return amqp_rpc_reply_t indicating success or failure
  *
  * \since v0.1
*)

function amqp_channel_close(state: amqp_connection_state_t; channel: amqp_channel_t; code: integer): amqp_rpc_reply_t; cdecl;

(* *
  * Closes the entire connection
  *
  * Implicitly closes all channels and informs the broker the connection
  * is being closed, after receiving acknowldgement from the broker it closes
  * the socket.
  *
  * \param [in] state the connection object
  * \param [in] code the reason code for closing the connection. AMQP_REPLY_SUCCESS is a good default.
  * \return amqp_rpc_reply_t indicating the result
  *
  * \since v0.1
*)

function amqp_connection_close(state: amqp_connection_state_t; code: integer): amqp_rpc_reply_t; cdecl;

(* *
  * Acknowledges a message
  *
  * Does a basic.ack on a received message
  *
  * \param [in] state the connection object
  * \param [in] channel the channel identifier
  * \param [in] delivery_tag the delivery tag of the message to be ack'd
  * \param [in] multiple if true, ack all messages up to this delivery tag, if
  *              false ack only this delivery tag
  * \return 0 on success,  0 > on failing to send the ack to the broker.
  *            this will not indicate failure if something goes wrong on the broker
  *
  * \since v0.1
*)

function amqp_basic_ack(state: amqp_connection_state_t; channel: amqp_channel_t; delivery_tag: UInt64; multiple: amqp_boolean_t): integer; cdecl;

(* *
  * Do a basic.get
  *
  * Synchonously polls the broker for a message in a queue, and
  * retrieves the message if a message is in the queue.
  *
  * \param [in] state the connection object
  * \param [in] channel the channel identifier to use
  * \param [in] queue the queue name to retrieve from
  * \param [in] no_ack if true the message is automatically ack'ed
  *              if false amqp_basic_ack should be called once the message
  *              retrieved has been processed
  * \return amqp_rpc_reply indicating success or failure
  *
  * \since v0.1
*)

function amqp_basic_get(state: amqp_connection_state_t; channel: amqp_channel_t; queue: amqp_bytes_t; no_ack: amqp_boolean_t): amqp_rpc_reply_t; cdecl;

(* *
  * Do a basic.reject
  *
  * Actively reject a message that has been delivered
  *
  * \param [in] state the connection object
  * \param [in] channel the channel identifier
  * \param [in] delivery_tag the delivery tag of the message to reject
  * \param [in] requeue indicate to the broker whether it should requeue the
  *              message or just discard it.
  * \return 0 on success, 0 > on failing to send the reject method to the broker.
  *          This will not indicate failure if something goes wrong on the broker.
  *
  * \since v0.1
*)

function amqp_basic_reject(state: amqp_connection_state_t; channel: amqp_channel_t; delivery_tag: UInt64; requeue: amqp_boolean_t): integer; cdecl;

(* *
  * Do a basic.nack
  *
  * Actively reject a message, this has the same effect as amqp_basic_reject()
  * however, amqp_basic_nack() can negatively acknowledge multiple messages with
  * one call much like amqp_basic_ack() can acknowledge mutliple messages with
  * one call.
  *
  * \param [in] state the connection object
  * \param [in] channel the channel identifier
  * \param [in] delivery_tag the delivery tag of the message to reject
  * \param [in] multiple if set to 1 negatively acknowledge all unacknowledged
  *              messages on this channel.
  * \param [in] requeue indicate to the broker whether it should requeue the
  *              message or dead-letter it.
  * \return AMQP_STATUS_OK on success, an amqp_status_enum value otherwise.
  *
  * \since v0.5.0
*)

function amqp_basic_nack(state: amqp_connection_state_t; channel: amqp_channel_t; delivery_tag: UInt64; multiple: amqp_boolean_t; requeue: amqp_boolean_t): integer; cdecl;
(* *
  * Check to see if there is data left in the receive buffer
  *
  * Can be used to see if there is data still in the buffer, if so
  * calling amqp_simple_wait_frame will not immediately enter a
  * blocking read.
  *
  * \param [in] state the connection object
  * \return true if there is data in the recieve buffer, false otherwise
  *
  * \since v0.1
*)

function amqp_data_in_buffer(state: amqp_connection_state_t): amqp_boolean_t; cdecl;

(* *
  * Get the error string for the given error code.
  *
  * \deprecated This function has been deprecated in favor of
  *  \ref amqp_error_string2() which returns statically allocated
  *  string which do not need to be freed by the caller.
  *
  * The returned string resides on the heap; the caller is responsible
  * for freeing it.
  *
  * \param [in] err return error code
  * \return the error string
  *
  * \since v0.1
*)
function amqp_error_string(err: integer): PAnsiChar; cdecl;

(* *
  * Get the error string for the given error code.
  *
  * Get an error string associated with an error code. The string is statically
  * allocated and does not need to be freed
  *
  * \param [in] err the error code
  * \return the error string
  *
  * \since v0.4.0
*)
function amqp_error_string2(err: integer): PAnsiChar; cdecl;

(* *
  * Deserialize an amqp_table_t from AMQP wireformat
  *
  * This is an internal function and is not typically used by
  * client applications
  *
  * \param [in] encoded the buffer containing the serialized data
  * \param [in] pool memory pool used to allocate the table entries from
  * \param [in] output the amqp_table_t structure to fill in. Any existing
  *             entries will be erased
  * \param [in,out] offset The offset into the encoded buffer to start
  *                 reading the serialized table. It will be updated
  *                 by this function to end of the table
  * \return AMQP_STATUS_OK on success, an amqp_status_enum value on failure
  *  Possible error codes:
  *  - AMQP_STATUS_NO_MEMORY out of memory
  *  - AMQP_STATUS_BAD_AMQP_DATA invalid wireformat
  *
  * \since v0.1
*)

function amqp_decode_table(encoded: amqp_bytes_t; pool: pamqp_pool_t; output: pamqp_table_t; var offset: size_t): integer; cdecl;

(* *
  * Serializes an amqp_table_t to the AMQP wireformat
  *
  * This is an internal function and is not typically used by
  * client applications
  *
  * \param [in] encoded the buffer where to serialize the table to
  * \param [in] input the amqp_table_t to serialize
  * \param [in,out] offset The offset into the encoded buffer to start
  *                 writing the serialized table. It will be updated
  *                 by this function to where writing left off
  * \return AMQP_STATUS_OK on success, an amqp_status_enum value on failure
  *  Possible error codes:
  *  - AMQP_STATUS_TABLE_TOO_BIG the serialized form is too large for the
  *    buffer
  *  - AMQP_STATUS_BAD_AMQP_DATA invalid table
  *
  * \since v0.1
*)

function amqp_encode_table(encoded: amqp_bytes_t; input: pamqp_table_t; var offset: size_t): integer; cdecl;

(* *
  * Create a deep-copy of an amqp_table_t object
  *
  * Creates a deep-copy of an amqp_table_t object, using the provided pool
  * object to allocate the necessary memory. This memory can be freed later by
  * call recycle_amqp_pool(), or empty_amqp_pool()
  *
  * \param [in] original the table to copy
  * \param [in,out] clone the table to copy to
  * \param [in] pool the initialized memory pool to do allocations for the table
  *             from
  * \return AMQP_STATUS_OK on success, amqp_status_enum value on failure.
  *  Possible error values:
  *  - AMQP_STATUS_NO_MEMORY - memory allocation failure.
  *  - AMQP_STATUS_INVALID_PARAMETER - invalid table (e.g., no key name)
  *
  * \since v0.4.0
*)

function amqp_table_clone(const original: pamqp_table_t; clone: pamqp_table_t; pool: pamqp_pool_t): integer; cdecl;

(* *
  * A message object
  *
  * \since v0.4.0
*)
type
  pamqp_message_t = ^amqp_message_t_;

  amqp_message_t_ = record
    properties: amqp_basic_properties_t; (* *< message properties *)
    body: amqp_bytes_t; (* *< message body *)
    pool: amqp_pool_t; (* *< pool used to allocate properties *)
  end;

  amqp_message_t = amqp_message_t_;

  (* *
    * Reads the next message on a channel
    *
    * Reads a complete message (header + body) on a specified channel. This
    * function is intended to be used with amqp_basic_get() or when an
    * AMQP_BASIC_DELIVERY_METHOD method is received.
    *
    * \param [in,out] state the connection object
    * \param [in] channel the channel on which to read the message from
    * \param [in,out] message a pointer to a amqp_message_t object. Caller should
    *                 call amqp_message_destroy() when it is done using the
    *                 fields in the message object.  The caller is responsible for
    *                 allocating/destroying the amqp_message_t object itself.
    * \param [in] flags pass in 0. Currently unused.
    * \returns a amqp_rpc_reply_t object. ret.reply_type == AMQP_RESPONSE_NORMAL on success.
    *
    * \since v0.4.0
  *)

function amqp_read_message(state: amqp_connection_state_t; channel: amqp_channel_t; message: pamqp_message_t; flags: integer): amqp_rpc_reply_t; cdecl;

(* *
  * Frees memory associated with a amqp_message_t allocated in amqp_read_message
  *
  * \param [in] message
  *
  * \since v0.4.0
*)

procedure amqp_destroy_message(&message: pamqp_message_t);

(* *
  * Envelope object
  *
  * \since v0.4.0
*)
type
  pamqp_envelope_t = ^amqp_envelope_t_;

  amqp_envelope_t_ = record
    channel: amqp_channel_t; (* *< channel message was delivered on *)
    consumer_tag: amqp_bytes_t; (* *< the consumer tag the message was delivered to *)
    delivery_tag: UInt64; (* *< the messages delivery tag *)
    redelivered: amqp_boolean_t; (* *< flag indicating whether this message is being redelivered *)
    exchange: amqp_bytes_t; (* *< exchange this message was published to *)
    routing_key: amqp_bytes_t; (* *< the routing key this message was published with *)
    &message: amqp_message_t; (* *< the message *)
  end;

  amqp_envelope_t = amqp_envelope_t_;

  (* *
    * Wait for and consume a message
    *
    * Waits for a basic.deliver method on any channel, upon receipt of
    * basic.deliver it reads that message, and returns. If any other method is
    * received before basic.deliver, this function will return an amqp_rpc_reply_t
    * with ret.reply_type == AMQP_RESPONSE_LIBRARY_EXCEPTION, and
    * ret.library_error == AMQP_STATUS_UNEXPECTED_FRAME. The caller should then
    * call amqp_simple_wait_frame() to read this frame and take appropriate action.
    *
    * This function should be used after starting a consumer with the
    * amqp_basic_consume() function
    *
    * \param [in,out] state the connection object
    * \param [in,out] envelope a pointer to a amqp_envelope_t object. Caller
    *                 should call #amqp_destroy_envelope() when it is done using
    *                 the fields in the envelope object. The caller is responsible
    *                 for allocating/destroying the amqp_envelope_t object itself.
    * \param [in] timeout a timeout to wait for a message delivery. Passing in
    *             NULL will result in blocking behavior.
    * \param [in] flags pass in 0. Currently unused.
    * \returns a amqp_rpc_reply_t object.  ret.reply_type == AMQP_RESPONSE_NORMAL
    *          on success. If ret.reply_type == AMQP_RESPONSE_LIBRARY_EXCEPTION, and
    *          ret.library_error == AMQP_STATUS_UNEXPECTED_FRAME, a frame other
    *          than AMQP_BASIC_DELIVER_METHOD was received, the caller should call
    *          amqp_simple_wait_frame() to read this frame and take appropriate
    *          action.
    *
    * \since v0.4.0
  *)

function amqp_consume_message(state: amqp_connection_state_t; envelope: pamqp_envelope_t; timeout: ptimeval; flags: integer): amqp_rpc_reply_t; cdecl;

(* *
  * Frees memory associated with a amqp_envelope_t allocated in amqp_consume_message()
  *
  * \param [in] envelope
  *
  * \since v0.4.0
*)

procedure amqp_destroy_envelope(envelope: pamqp_envelope_t); cdecl;

(* *
  * Parameters used to connect to the RabbitMQ broker
  *
  * \since v0.2
*)
type
  pamqp_connection_info = ^amqp_connection_info;

  amqp_connection_info = record
    user: PAnsiChar; (* *< the username to authenticate with the broker, default on most broker is 'guest' *)
    password: PAnsiChar; (* *< the password to authenticate with the broker, default on most brokers is 'guest' *)
    host: PAnsiChar; (* *< the hostname of the broker *)
    vhost: PAnsiChar; (* *< the virtual host on the broker to connect to, a good default is "/" *)
    port: integer; (* *< the port that the broker is listening on, default on most brokers is 5672 *)
    SSL: amqp_boolean_t;
  end;
  (* *
    * Initialze an amqp_connection_info to default values
    *
    * The default values are:
    * - user: "guest"
    * - password: "guest"
    * - host: "localhost"
    * - vhost: "/"
    * - port: 5672
    *
    * \param [out] parsed the connection info to set defaults on
    *
    * \since v0.2
  *)

procedure amqp_default_connection_info(parsed: pamqp_connection_info); cdecl;

(* *
  * Parse a connection URL
  *
  * An amqp connection url takes the form:
  *
  * amqp://[$USERNAME[:$PASSWORD]\@]$HOST[:$PORT]/[$VHOST]
  *
  * Examples:
  *  amqp://guest:guest\@localhost:5672//
  *  amqp://guest:guest\@localhost/myvhost
  *
  *  Any missing parts of the URL will be set to the defaults specified in
  *  amqp_default_connection_info. For amqps: URLs the default port will be set
  *  to 5671 instead of 5672 for non-SSL URLs.
  *
  * \note This function modifies url parameter.
  *
  * \param [in] url URI to parse, note that this parameter is modified by the
  *             function.
  * \param [out] parsed the connection info gleaned from the URI. The char*
  *              members will point to parts of the url input parameter.
  *              Memory management will depend on how the url is allocated.
  * \returns AMQP_STATUS_OK on success, AMQP_STATUS_BAD_URL on failure
  *
  * \since v0.2
*)

function amqp_parse_url(URL: PAnsiChar; parsed: pamqp_connection_info): integer; cdecl;

(* socket API *)

(* *
  * Open a socket connection.
  *
  * This function opens a socket connection returned from amqp_tcp_socket_new()
  * or amqp_ssl_socket_new(). This function should be called after setting
  * socket options and prior to assigning the socket to an AMQP connection with
  * amqp_set_socket().
  *
  * \param [in,out] self A socket object.
  * \param [in] host Connect to this host.
  * \param [in] port Connect on this remote port.
  *
  * \return AMQP_STATUS_OK on success, an amqp_status_enum on failure
  *
  * \since v0.4.0
*)

function amqp_socket_open(self: pamqp_socket_t; const host: PAnsiChar; port: integer): integer; cdecl;

(* *
  * Open a socket connection.
  *
  * This function opens a socket connection returned from amqp_tcp_socket_new()
  * or amqp_ssl_socket_new(). This function should be called after setting
  * socket options and prior to assigning the socket to an AMQP connection with
  * amqp_set_socket().
  *
  * \param [in,out] self A socket object.
  * \param [in] host Connect to this host.
  * \param [in] port Connect on this remote port.
  * \param [in] timeout Max allowed time to spent on opening. If NULL - run in blocking mode
  *
  * \return AMQP_STATUS_OK on success, an amqp_status_enum on failure.
  *
  * \since v0.4.0
*)

function amqp_socket_open_noblock(self: pamqp_socket_t; const host: PAnsiChar; port: integer; timeout: ptimeval): integer; cdecl;

(* *
  * Get the socket descriptor in use by a socket object.
  *
  * Retrieve the underlying socket descriptor. This function can be used to
  * perform low-level socket operations that aren't supported by the socket
  * interface. Use with caution!
  *
  * \param [in,out] self A socket object.
  *
  * \return The underlying socket descriptor, or -1 if there is no socket descriptor
  *  associated with
  *  with
  *
  * \since v0.4.0
*)

function amqp_socket_get_sockfd(self: pamqp_socket_t): integer; cdecl;

(* *
  * Get the socket object associated with a amqp_connection_state_t
  *
  * \param [in] state the connection object to get the socket from
  * \return a pointer to the socket object, or NULL if one has not been assigned
  *
  * \since v0.4.0
*)

function amqp_get_socket(state: amqp_connection_state_t): pamqp_socket_t; cdecl;

(* *
  * Get the broker properties table
  *
  * \param [in] state the connection object
  * \return a pointer to an amqp_table_t containing the properties advertised
  *  by the broker on connection. The connection object owns the table, it
  *  should not be modified.
  *
  * \since v0.5.0
*)

function amqp_get_server_properties(state: amqp_connection_state_t): pamqp_table_t; cdecl;

(* *
  * Get the client properties table
  *
  * Get the properties that were passed to the broker on connection.
  *
  * \param [in] state the connection object
  * \return a pointer to an amqp_table_t containing the properties advertised
  *  by the client on connection. The connection object owns the table, it
  *  should not be modified.
  *
  * \since v0.7.0
*)

function amqp_get_client_properties(state: amqp_connection_state_t): pamqp_table_t; cdecl;
{$ENDREGION}
{$REGION 'ampq_private.h'}

const
  AMQ_COPYRIGHT = 'Copyright (c) 2007-2014 VMWare Inc, Tony Garnock-Jones,'' and Alan Antonuk.';

  (*
    * Connection states: XXX FIX THIS
    *
    * - CONNECTION_STATE_INITIAL: The initial state, when we cannot be
    *   sure if the next thing we will get is the first AMQP frame, or a
    *   protocol header from the server.
    *
    * - CONNECTION_STATE_IDLE: The normal state between
    *   frames. Connections may only be reconfigured, and the
    *   connection's pools recycled, when in this state. Whenever we're
    *   in this state, the inbound_buffer's bytes pointer must be NULL;
    *   any other state, and it must point to a block of memory allocated
    *   from the frame_pool.
    *
    * - CONNECTION_STATE_HEADER: Some bytes of an incoming frame have
    *   been seen, but not a complete frame header's worth.
    *
    * - CONNECTION_STATE_BODY: A complete frame header has been seen, but
    *   the frame is not yet complete. When it is completed, it will be
    *   returned, and the connection will return to IDLE state.
    *
  *)
type

  amqp_status_private_enum_ = (
    // 0x00xx -> AMQP_STATUS_
    // 0x01xx -> AMQP_STATUS_TCP_*
    // 0x02xx -> AMQP_STATUS_SSL_*
    AMQP_PRIVATE_STATUS_SOCKET_NEEDREAD = -$1301, AMQP_PRIVATE_STATUS_SOCKET_NEEDWRITE = -$1302);
  amqp_status_private_enum = amqp_status_private_enum_;

{$ENDREGION}
{$REGION 'amqp_tcp_socket.h'}
function amqp_tcp_socket_new(state: amqp_connection_state_t): pamqp_socket_t; cdecl;

(* *
  * Create a new TCP socket.
  *
  * Call amqp_connection_close() to release socket resources.
  *
  * \return A new socket object or NULL if an error occurred.
  *
  * \since v0.4.0
*)

(* *
  * Assign an open file descriptor to a socket object.
  *
  * This function must not be used in conjunction with amqp_socket_open(), i.e.
  * the socket connection should already be open(2) when this function is
  * called.
  *
  * \param [in,out] self A TCP socket object.
  * \param [in] sockfd An open socket descriptor.
  *
  * \since v0.4.0
*)

procedure amqp_tcp_socket_set_sockfd(self: pamqp_socket_t; sockfd: integer);
{$ENDREGION}

implementation

function amqp_constant_name; external LIBFILE;

function amqp_constant_is_hard_error; external LIBFILE;

function amqp_method_name; external LIBFILE;

function amqp_method_has_content; external LIBFILE;

function amqp_decode_method; external LIBFILE;

function amqp_decode_properties; external LIBFILE;

function amqp_encode_method; external LIBFILE;

function amqp_encode_properties; external LIBFILE;

function amqp_channel_open; external LIBFILE;

function amqp_channel_flow; external LIBFILE;
function amqp_exchange_declare; external LIBFILE;
function amqp_exchange_delete; external LIBFILE;
function amqp_exchange_bind; external LIBFILE;
function amqp_exchange_unbind; external LIBFILE;
function amqp_queue_declare; external LIBFILE;
function amqp_queue_bind; external LIBFILE;
function amqp_queue_purge; external LIBFILE;
function amqp_queue_delete; external LIBFILE;
function amqp_queue_unbind; external LIBFILE;
function amqp_basic_qos; external LIBFILE;
function amqp_basic_consume; external LIBFILE;
function amqp_basic_cancel; external LIBFILE;
function amqp_basic_recover; external LIBFILE;
function amqp_tx_select; external LIBFILE;
function amqp_tx_commit; external LIBFILE;
function amqp_tx_rollback; external LIBFILE;
function amqp_confirm_select; external LIBFILE;

function amqp_version_number; external LIBFILE;

function amqp_version; external LIBFILE;

procedure init_amqp_pool; external LIBFILE;

procedure recycle_amqp_pool; external LIBFILE;

procedure empty_amqp_pool; external LIBFILE;

function amqp_pool_alloc; external LIBFILE;

procedure amqp_pool_alloc_bytes; external LIBFILE;

function amqp_cstring_bytes; external LIBFILE;

function amqp_bytes_malloc_dup; external LIBFILE;

function amqp_bytes_malloc; external LIBFILE;

procedure amqp_bytes_free; external LIBFILE;

function amqp_new_connection; external LIBFILE;

function amqp_get_sockfd; external LIBFILE;

procedure amqp_set_sockfd; external LIBFILE;

function amqp_tune_connection; external LIBFILE;

function amqp_get_channel_max; external LIBFILE;

function amqp_get_frame_max; external LIBFILE;

function amqp_get_heartbeat; external LIBFILE;

function amqp_destroy_connection; external LIBFILE;

function amqp_handle_input; external LIBFILE;

function amqp_release_buffers_ok; external LIBFILE;

procedure amqp_release_buffers; external LIBFILE;

procedure amqp_maybe_release_buffers; external LIBFILE;

procedure amqp_maybe_release_buffers_on_channel; external LIBFILE;

function amqp_send_frame; external LIBFILE;

function amqp_table_entry_cmp; external LIBFILE;

function amqp_open_socket; external LIBFILE;

function amqp_send_header; external LIBFILE;

function amqp_frames_enqueued; external LIBFILE;

function amqp_simple_wait_frame; external LIBFILE;

function amqp_simple_wait_frame_noblock; external LIBFILE;

function amqp_simple_wait_method; external LIBFILE;

function amqp_send_method; external LIBFILE;

function amqp_simple_rpc; external LIBFILE;

function amqp_simple_rpc_decoded; external LIBFILE;

function amqp_get_rpc_reply; external LIBFILE;

function amqp_login; external LIBFILE;

function amqp_login_with_properties; external LIBFILE;

function amqp_basic_publish; external LIBFILE;

function amqp_channel_close; external LIBFILE;

function amqp_connection_close; external LIBFILE;

function amqp_basic_ack; external LIBFILE;

function amqp_basic_get; external LIBFILE;

function amqp_basic_reject; external LIBFILE;

function amqp_basic_nack; external LIBFILE;

function amqp_data_in_buffer; external LIBFILE;

function amqp_error_string; external LIBFILE;

function amqp_error_string2; external LIBFILE;

function amqp_decode_table; external LIBFILE;

function amqp_encode_table; external LIBFILE;

function amqp_table_clone; external LIBFILE;

function amqp_read_message; external LIBFILE;

procedure amqp_destroy_message; external LIBFILE;

function amqp_consume_message; external LIBFILE;

procedure amqp_destroy_envelope; external LIBFILE;

procedure amqp_default_connection_info; external LIBFILE;

function amqp_parse_url; external LIBFILE;

function amqp_socket_open; external LIBFILE;

function amqp_socket_open_noblock; external LIBFILE;

function amqp_socket_get_sockfd; external LIBFILE;

function amqp_get_socket; external LIBFILE;

function amqp_get_server_properties; external LIBFILE;

function amqp_get_client_properties; external LIBFILE;
function amqp_tcp_socket_new; external LIBFILE;

procedure amqp_tcp_socket_set_sockfd; external LIBFILE;

end.
