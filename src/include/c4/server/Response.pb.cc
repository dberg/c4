// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: Response.proto

#define INTERNAL_SUPPRESS_PROTOBUF_FIELD_DEPRECATION
#include "Response.pb.h"

#include <algorithm>

#include <google/protobuf/stubs/common.h>
#include <google/protobuf/stubs/once.h>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/wire_format_lite_inl.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/generated_message_reflection.h>
#include <google/protobuf/reflection_ops.h>
#include <google/protobuf/wire_format.h>
// @@protoc_insertion_point(includes)

namespace c4 {

namespace {

const ::google::protobuf::Descriptor* Response_descriptor_ = NULL;
const ::google::protobuf::internal::GeneratedMessageReflection*
  Response_reflection_ = NULL;
const ::google::protobuf::EnumDescriptor* Response_ResponseCode_descriptor_ = NULL;

}  // namespace


void protobuf_AssignDesc_Response_2eproto() {
  protobuf_AddDesc_Response_2eproto();
  const ::google::protobuf::FileDescriptor* file =
    ::google::protobuf::DescriptorPool::generated_pool()->FindFileByName(
      "Response.proto");
  GOOGLE_CHECK(file != NULL);
  Response_descriptor_ = file->message_type(0);
  static const int Response_offsets_[2] = {
    GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(Response, code_),
    GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(Response, body_),
  };
  Response_reflection_ =
    new ::google::protobuf::internal::GeneratedMessageReflection(
      Response_descriptor_,
      Response::default_instance_,
      Response_offsets_,
      GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(Response, _has_bits_[0]),
      GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(Response, _unknown_fields_),
      -1,
      ::google::protobuf::DescriptorPool::generated_pool(),
      ::google::protobuf::MessageFactory::generated_factory(),
      sizeof(Response));
  Response_ResponseCode_descriptor_ = Response_descriptor_->enum_type(0);
}

namespace {

GOOGLE_PROTOBUF_DECLARE_ONCE(protobuf_AssignDescriptors_once_);
inline void protobuf_AssignDescriptorsOnce() {
  ::google::protobuf::GoogleOnceInit(&protobuf_AssignDescriptors_once_,
                 &protobuf_AssignDesc_Response_2eproto);
}

void protobuf_RegisterTypes(const ::std::string&) {
  protobuf_AssignDescriptorsOnce();
  ::google::protobuf::MessageFactory::InternalRegisterGeneratedMessage(
    Response_descriptor_, &Response::default_instance());
}

}  // namespace

void protobuf_ShutdownFile_Response_2eproto() {
  delete Response::default_instance_;
  delete Response_reflection_;
}

void protobuf_AddDesc_Response_2eproto() {
  static bool already_here = false;
  if (already_here) return;
  already_here = true;
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  ::google::protobuf::DescriptorPool::InternalAddGeneratedFile(
    "\n\016Response.proto\022\002c4\"d\n\010Response\022\'\n\004code"
    "\030\001 \002(\0162\031.c4.Response.ResponseCode\022\014\n\004bod"
    "y\030\002 \001(\t\"!\n\014ResponseCode\022\006\n\002OK\020\000\022\t\n\005ERROR"
    "\020\001", 122);
  ::google::protobuf::MessageFactory::InternalRegisterGeneratedFile(
    "Response.proto", &protobuf_RegisterTypes);
  Response::default_instance_ = new Response();
  Response::default_instance_->InitAsDefaultInstance();
  ::google::protobuf::internal::OnShutdown(&protobuf_ShutdownFile_Response_2eproto);
}

// Force AddDescriptors() to be called at static initialization time.
struct StaticDescriptorInitializer_Response_2eproto {
  StaticDescriptorInitializer_Response_2eproto() {
    protobuf_AddDesc_Response_2eproto();
  }
} static_descriptor_initializer_Response_2eproto_;

// ===================================================================

const ::google::protobuf::EnumDescriptor* Response_ResponseCode_descriptor() {
  protobuf_AssignDescriptorsOnce();
  return Response_ResponseCode_descriptor_;
}
bool Response_ResponseCode_IsValid(int value) {
  switch(value) {
    case 0:
    case 1:
      return true;
    default:
      return false;
  }
}

#ifndef _MSC_VER
const Response_ResponseCode Response::OK;
const Response_ResponseCode Response::ERROR;
const Response_ResponseCode Response::ResponseCode_MIN;
const Response_ResponseCode Response::ResponseCode_MAX;
const int Response::ResponseCode_ARRAYSIZE;
#endif  // _MSC_VER
#ifndef _MSC_VER
const int Response::kCodeFieldNumber;
const int Response::kBodyFieldNumber;
#endif  // !_MSC_VER

Response::Response()
  : ::google::protobuf::Message() {
  SharedCtor();
}

void Response::InitAsDefaultInstance() {
}

Response::Response(const Response& from)
  : ::google::protobuf::Message() {
  SharedCtor();
  MergeFrom(from);
}

void Response::SharedCtor() {
  _cached_size_ = 0;
  code_ = 0;
  body_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  ::memset(_has_bits_, 0, sizeof(_has_bits_));
}

Response::~Response() {
  SharedDtor();
}

void Response::SharedDtor() {
  if (body_ != &::google::protobuf::internal::kEmptyString) {
    delete body_;
  }
  if (this != default_instance_) {
  }
}

void Response::SetCachedSize(int size) const {
  GOOGLE_SAFE_CONCURRENT_WRITES_BEGIN();
  _cached_size_ = size;
  GOOGLE_SAFE_CONCURRENT_WRITES_END();
}
const ::google::protobuf::Descriptor* Response::descriptor() {
  protobuf_AssignDescriptorsOnce();
  return Response_descriptor_;
}

const Response& Response::default_instance() {
  if (default_instance_ == NULL) protobuf_AddDesc_Response_2eproto();
  return *default_instance_;
}

Response* Response::default_instance_ = NULL;

Response* Response::New() const {
  return new Response;
}

void Response::Clear() {
  if (_has_bits_[0 / 32] & (0xffu << (0 % 32))) {
    code_ = 0;
    if (has_body()) {
      if (body_ != &::google::protobuf::internal::kEmptyString) {
        body_->clear();
      }
    }
  }
  ::memset(_has_bits_, 0, sizeof(_has_bits_));
  mutable_unknown_fields()->Clear();
}

bool Response::MergePartialFromCodedStream(
    ::google::protobuf::io::CodedInputStream* input) {
#define DO_(EXPRESSION) if (!(EXPRESSION)) return false
  ::google::protobuf::uint32 tag;
  while ((tag = input->ReadTag()) != 0) {
    switch (::google::protobuf::internal::WireFormatLite::GetTagFieldNumber(tag)) {
      // required .c4.Response.ResponseCode code = 1;
      case 1: {
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_VARINT) {
          int value;
          DO_((::google::protobuf::internal::WireFormatLite::ReadPrimitive<
                   int, ::google::protobuf::internal::WireFormatLite::TYPE_ENUM>(
                 input, &value)));
          if (::c4::Response_ResponseCode_IsValid(value)) {
            set_code(static_cast< ::c4::Response_ResponseCode >(value));
          } else {
            mutable_unknown_fields()->AddVarint(1, value);
          }
        } else {
          goto handle_uninterpreted;
        }
        if (input->ExpectTag(18)) goto parse_body;
        break;
      }

      // optional string body = 2;
      case 2: {
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_LENGTH_DELIMITED) {
         parse_body:
          DO_(::google::protobuf::internal::WireFormatLite::ReadString(
                input, this->mutable_body()));
          ::google::protobuf::internal::WireFormat::VerifyUTF8String(
            this->body().data(), this->body().length(),
            ::google::protobuf::internal::WireFormat::PARSE);
        } else {
          goto handle_uninterpreted;
        }
        if (input->ExpectAtEnd()) return true;
        break;
      }

      default: {
      handle_uninterpreted:
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_END_GROUP) {
          return true;
        }
        DO_(::google::protobuf::internal::WireFormat::SkipField(
              input, tag, mutable_unknown_fields()));
        break;
      }
    }
  }
  return true;
#undef DO_
}

void Response::SerializeWithCachedSizes(
    ::google::protobuf::io::CodedOutputStream* output) const {
  // required .c4.Response.ResponseCode code = 1;
  if (has_code()) {
    ::google::protobuf::internal::WireFormatLite::WriteEnum(
      1, this->code(), output);
  }

  // optional string body = 2;
  if (has_body()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->body().data(), this->body().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    ::google::protobuf::internal::WireFormatLite::WriteString(
      2, this->body(), output);
  }

  if (!unknown_fields().empty()) {
    ::google::protobuf::internal::WireFormat::SerializeUnknownFields(
        unknown_fields(), output);
  }
}

::google::protobuf::uint8* Response::SerializeWithCachedSizesToArray(
    ::google::protobuf::uint8* target) const {
  // required .c4.Response.ResponseCode code = 1;
  if (has_code()) {
    target = ::google::protobuf::internal::WireFormatLite::WriteEnumToArray(
      1, this->code(), target);
  }

  // optional string body = 2;
  if (has_body()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->body().data(), this->body().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    target =
      ::google::protobuf::internal::WireFormatLite::WriteStringToArray(
        2, this->body(), target);
  }

  if (!unknown_fields().empty()) {
    target = ::google::protobuf::internal::WireFormat::SerializeUnknownFieldsToArray(
        unknown_fields(), target);
  }
  return target;
}

int Response::ByteSize() const {
  int total_size = 0;

  if (_has_bits_[0 / 32] & (0xffu << (0 % 32))) {
    // required .c4.Response.ResponseCode code = 1;
    if (has_code()) {
      total_size += 1 +
        ::google::protobuf::internal::WireFormatLite::EnumSize(this->code());
    }

    // optional string body = 2;
    if (has_body()) {
      total_size += 1 +
        ::google::protobuf::internal::WireFormatLite::StringSize(
          this->body());
    }

  }
  if (!unknown_fields().empty()) {
    total_size +=
      ::google::protobuf::internal::WireFormat::ComputeUnknownFieldsSize(
        unknown_fields());
  }
  GOOGLE_SAFE_CONCURRENT_WRITES_BEGIN();
  _cached_size_ = total_size;
  GOOGLE_SAFE_CONCURRENT_WRITES_END();
  return total_size;
}

void Response::MergeFrom(const ::google::protobuf::Message& from) {
  GOOGLE_CHECK_NE(&from, this);
  const Response* source =
    ::google::protobuf::internal::dynamic_cast_if_available<const Response*>(
      &from);
  if (source == NULL) {
    ::google::protobuf::internal::ReflectionOps::Merge(from, this);
  } else {
    MergeFrom(*source);
  }
}

void Response::MergeFrom(const Response& from) {
  GOOGLE_CHECK_NE(&from, this);
  if (from._has_bits_[0 / 32] & (0xffu << (0 % 32))) {
    if (from.has_code()) {
      set_code(from.code());
    }
    if (from.has_body()) {
      set_body(from.body());
    }
  }
  mutable_unknown_fields()->MergeFrom(from.unknown_fields());
}

void Response::CopyFrom(const ::google::protobuf::Message& from) {
  if (&from == this) return;
  Clear();
  MergeFrom(from);
}

void Response::CopyFrom(const Response& from) {
  if (&from == this) return;
  Clear();
  MergeFrom(from);
}

bool Response::IsInitialized() const {
  if ((_has_bits_[0] & 0x00000001) != 0x00000001) return false;

  return true;
}

void Response::Swap(Response* other) {
  if (other != this) {
    std::swap(code_, other->code_);
    std::swap(body_, other->body_);
    std::swap(_has_bits_[0], other->_has_bits_[0]);
    _unknown_fields_.Swap(&other->_unknown_fields_);
    std::swap(_cached_size_, other->_cached_size_);
  }
}

::google::protobuf::Metadata Response::GetMetadata() const {
  protobuf_AssignDescriptorsOnce();
  ::google::protobuf::Metadata metadata;
  metadata.descriptor = Response_descriptor_;
  metadata.reflection = Response_reflection_;
  return metadata;
}


// @@protoc_insertion_point(namespace_scope)

}  // namespace c4

// @@protoc_insertion_point(global_scope)
