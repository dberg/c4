#include <iostream>
#include <djp/Parser.h>
using namespace djp;

int main() {
  std::string myClass =
    "class Foo {"
    "    public Foo() {}"
    "    public String bar() { return ""; }"
    "    public void baz() {}"
    "}";

  Parser parser("MyClass.java", myClass);
  parser.parse();
  spNormalClassDeclaration nClassDecl = parser.compilationUnit
    ->typeDecls[0]
    ->classOrIntDecl
    ->classDecl
    ->nClassDecl;
  std::string className = nClassDecl->id->value;
  std::cout << "Class " << className << std::endl;

  spClassBody classBody = nClassDecl->classBody;
  for (std::size_t i = 0; i < classBody->classBodyDecls.size(); i++) {
    spMemberDecl memberDecl =
      classBody->classBodyDecls[i]
      ->memberDecl;

    // Constructor
    if (memberDecl->opt
      == MemberDecl::OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST) {
      std::string methodName = memberDecl->id->value;
      std::cout << "  " << methodName << " (Constructor)" << std::endl;
      continue;
    }

    // Method
    if (memberDecl->opt == MemberDecl::OPT_METHOD_OR_FIELD_DECL) {
      std::string methodName = memberDecl->methodOrFieldDecl->id->value;
      std::cout << "  " << methodName << std::endl;
      continue;
    }

    // Void method
    if (memberDecl->opt
      == MemberDecl::OPT_VOID_IDENTIFIER_VOID_METHOD_DECLARATOR_REST) {
      std::string methodName = memberDecl->id->value;
      std::cout << "  " << methodName << std::endl;
      continue;
    }
  }
}
