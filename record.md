# Pascal Record 类型支持实现计划（更新版）

## 1. 概述

本计划详细说明了如何在 Simple Pascal Interpreter (SPI) 中实现对 Pascal record 类型的支持，包括常规记录和变体记录。Record 类型是 Pascal 中重要的复合数据类型，允许将不同类型的数据组合成一个单一实体。此更新版计划包含对变体记录语法格式、BNF语法和解析过程、RecordObject初始化方式以及解释器中使用RecordTypeSymbol的代码的重要修改。

## 2. 当前状态分析

### 2.1 已支持的类型
- 基本类型：Integer, Real, Boolean, Char, String
- 复合类型：Array, Enum
- 控制结构：If, While, For, Case
- 程序结构：Program, Block, Procedure, Function

### 2.2 示例文件分析
- `05_record_types.pas`：展示了基本记录类型的定义和使用
- `07_variant_records.pas`：展示了变体记录类型的定义和使用

### 2.3 缺失的 Record 支持
目前解释器缺少对 record 类型的完整支持，包括：
- Record 类型定义的语法解析
- Record 字段的访问和赋值
- Record 变量的初始化
- Record 作为参数传递和返回值
- 变体记录的特殊处理

## 3. 实现计划

### 3.1 词法分析器扩展

#### 3.1.1 添加新的 Token 类型
```python
# 在 spi.py 的 TokenType 枚举中添加
class TokenType(Enum):
    # 必须放置在 PROGRAM之后， END之前
    RECORD = "RECORD"
    CASE = "CASE"
    OF = "OF"
```

#### 3.1.2 更新词法分析器
```python
# 在 Lexer 类中添加对 RECORD 关键字的识别
# lexer使用 __id 来识别关键字
# 必须放置在 PROGRAM之后， END之前 , 才能被正确地加载到保留字字典里
    def __id(self) -> Token:
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        value = ""
        while self.current_char is not None and self.current_char.isalnum():
            value += self.current_char
            self.advance()

        token_type = RESERVED_KEYWORDS.get(value.upper())
        if token_type is None:
            token.type = TokenType.ID
            token.value = value
        else:
            # reserved keyword
            token.type = token_type
            token.value = value.upper()

        return token
```

### 3.2 语法分析器扩展

#### 3.2.1 添加新的 AST 节点类
```python
# 在 spi.py 中添加以下 AST 节点类

class RecordType(AST):
    """表示记录类型定义，包含常规字段和可选的变体部分"""
    def __init__(self, fields: list[RecordField], variant_part: Optional[VariantPart] = None):
        self.fields = fields  # 常规字段列表
        self.variant_part = variant_part  # 可选的变体部分

class RecordField(AST):
    """表示记录中的字段"""
    def __init__(self, name: Var, type_node: AST):
        self.name = name  # 字段名
        self.type_node = type_node  # 字段类型

class VariantPart(AST):
    """表示记录的变体部分"""
    def __init__(self, tag_field: Var, variant_cases: list[VariantCase]):
        self.tag_field = tag_field  # 标签字段（必须是枚举类型）
        self.variant_cases = variant_cases  # 变体情况列表

class VariantCase(AST):
    """表示变体记录中的一个变体情况"""
    def __init__(self, tag_values: list[str], fields: list[RecordField]):
        self.tag_values = tag_values  # 此变体情况对应的标签值列表
        self.fields = fields  # 该变体的字段列表
```

#### 3.2.2 更新 BNF 语法
```python
# 在 Parser 类的 parse 方法文档字符串中更新 BNF 语法

# 添加以下语法规则：
# type_spec : primitive_type_spec | string_type_spec | array_type_spec | record_type_spec | enum_type_spec

# record_type_spec : RECORD field_list variant_part END

# field_list : (field_declaration SEMI)*

# field_declaration : ID (COMMA ID)* COLON type_spec

# variant_part : CASE ID OF variant_case_list

# variant_case_list : variant_case (SEMI variant_case)*

# variant_case : ID (COMMA ID)* COLON LPAREN field_list RPAREN

# ID : 标识字段名，必须是已定义的枚举类型
```

#### 3.2.3 实现解析方法
```python
# 在 Parser 类中添加以下方法

def record_type_spec(self) -> RecordType:
    """解析记录类型定义，包括常规字段和可选的变体部分"""
    self.eat(TokenType.RECORD)
    fields = []
    
    # 解析固定部分字段
    while self.current_token.type != TokenType.CASE and self.current_token.type != TokenType.END:
        # 解析字段声明
        var_nodes = [Var(self.current_token)]
        self.eat(TokenType.ID)
        
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(TokenType.ID)
        
        self.eat(TokenType.COLON)
        type_node = self.type_spec()
        
        # 为每个变量创建字段节点
        for var_node in var_nodes:
            fields.append(RecordField(var_node, type_node))
        
        if self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
    
    # 解析变体部分（可选）
    variant_part = None
    if self.current_token.type == TokenType.CASE:
        variant_part = self._parse_variant_part()
    
    self.eat(TokenType.END)
    return RecordType(fields, variant_part)

def _parse_variant_part(self) -> VariantPart:
    """解析记录的变体部分，使用 'case kind of' 语法格式"""
    self.eat(TokenType.CASE)
    
    # 解析标签字段（必须是枚举类型）
    tag_field = Var(self.current_token)
    self.eat(TokenType.ID)
    
    self.eat(TokenType.OF)
    
    # 解析变体情况列表
    variant_cases = []
    while self.current_token.type != TokenType.END:
        # 解析标签值列表
        tag_values = [self.current_token.value]
        self.eat(TokenType.ID)
        
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            tag_values.append(self.current_token.value)
            self.eat(TokenType.ID)
        
        self.eat(TokenType.COLON)
        self.eat(TokenType.LPAREN)
        
        # 解析变体字段列表
        variant_fields = []
        while self.current_token.type != TokenType.RPAREN:
            # 解析字段声明（与 record_type_spec 相同）
            var_nodes = [Var(self.current_token)]
            self.eat(TokenType.ID)
            
            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                var_nodes.append(Var(self.current_token))
                self.eat(TokenType.ID)
            
            self.eat(TokenType.COLON)
            type_node = self.type_spec()
            
            for var_node in var_nodes:
                variant_fields.append(RecordField(var_node, type_node))
            
            if self.current_token.type == TokenType.SEMI:
                self.eat(TokenType.SEMI)
        
        self.eat(TokenType.RPAREN)
        
        # 创建变体情况
        variant_case = VariantCase(tag_values, variant_fields)
        variant_cases.append(variant_case)
        
        if self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
    
    return VariantPart(tag_field, variant_cases)
```

### 3.3 符号系统扩展

#### 3.3.1 添加新的符号类
```python
# 在 spi.py 中添加以下符号类

class RecordTypeSymbol(TypeSymbol):
    """表示记录类型符号"""
    def __init__(self, name: str, fields: dict[str, Symbol], variant_part: Optional[VariantPartSymbol] = None):
        super().__init__(name)
        self.fields = fields  # 字段名到字段符号的映射
        self.variant_part = variant_part  # 可选的变体部分符号

class RecordFieldSymbol(Symbol):
    """表示记录字段符号"""
    def __init__(self, name: str, type_symbol: TypeSymbol):
        super().__init__(name)
        self.type = type_symbol

class VariantPartSymbol:
    """表示记录变体部分的符号"""
    def __init__(self, tag_field: str, tag_type: TypeSymbol, variant_cases: dict[str, dict[str, Symbol]]):
        self.tag_field = tag_field  # 标签字段名
        self.tag_type = tag_type  # 标签字段类型（必须是枚举类型）
        self.variant_cases = variant_cases  # 标签值到变体字段符号的映射
```

#### 3.3.2 扩展语义分析器
```python
# 在 SemanticAnalyzer 类中添加以下方法

def visit_RecordType(self, node: RecordType) -> None:
    """访问记录类型定义节点"""
    # 创建记录类型符号
    type_name = f"record_{self._get_record_type_counter()}"
    fields = {}
    
    # 处理常规字段
    for field in node.fields:
        field_name = field.name.value
        self.visit(field.type_node)
        field_type = self.current_type
        
        # 创建字段符号并添加到记录中
        field_symbol = RecordFieldSymbol(field_name, field_type)
        fields[field_name] = field_symbol
    
    # 处理变体部分（如果存在）
    variant_part_symbol = None
    if node.variant_part:
        variant_part_symbol = self._process_variant_part(node.variant_part, fields)
    
    # 创建记录类型符号
    record_type_symbol = RecordTypeSymbol(type_name, fields, variant_part_symbol)
    self.current_type = record_type_symbol

def _process_variant_part(self, variant_part: VariantPart, existing_fields: dict[str, Symbol]) -> VariantPartSymbol:
    """处理记录的变体部分"""
    tag_field_name = variant_part.tag_field.value
    
    # 检查标签字段是否是已定义的枚举类型
    if tag_field_name not in existing_fields:
        self.error(
            error_code=ErrorCode.SEMANTIC_ERROR,
            token=variant_part.tag_field.token,
            message=f"Tag field '{tag_field_name}' must be defined in the record's field list"
        )
    
    tag_field_symbol = existing_fields[tag_field_name]
    if not isinstance(tag_field_symbol.type, EnumTypeSymbol):
        self.error(
            error_code=ErrorCode.SEMANTIC_ERROR,
            token=variant_part.tag_field.token,
            message=f"Tag field '{tag_field_name}' must be of enum type"
        )
    
    # 处理变体情况
    variant_cases = {}
    for variant_case in variant_part.variant_cases:
        # 检查标签值是否有效
        for tag_value in variant_case.tag_values:
            if tag_value not in tag_field_symbol.type.values:
                self.error(
                    error_code=ErrorCode.SEMANTIC_ERROR,
                    token=variant_part.tag_field.token,
                    message=f"Invalid tag value '{tag_value}' for enum type '{tag_field_symbol.type.name}'"
                )
        
        # 处理变体字段
        variant_fields = {}
        for field in variant_case.fields:
            field_name = field.name.value
            self.visit(field.type_node)
            field_type = self.current_type
            
            # 创建字段符号并添加到变体字段中
            field_symbol = RecordFieldSymbol(field_name, field_type)
            variant_fields[field_name] = field_symbol
        
        # 将变体字段添加到对应的标签值
        for tag_value in variant_case.tag_values:
            variant_cases[tag_value] = variant_fields
    
    return VariantPartSymbol(tag_field_name, tag_field_symbol.type, variant_cases)

def _get_record_type_counter(self) -> int:
    """获取记录类型计数器，用于生成唯一的记录类型名称"""
    if not hasattr(self, '_record_type_counter'):
        self._record_type_counter = 0
    self._record_type_counter += 1
    return self._record_type_counter
```

### 3.4 解释器扩展

#### 3.4.1 添加新的对象类
```python
# 在 spi.py 中添加以下对象类

# !!! 需要修改， 直接传递 RecordType给 RecordObject
class RecordObject(Object):
    """表示记录对象，不依赖RecordTypeSymbol进行初始化"""
    def __init__(self, fields: dict[str, Object] = None, variant_part: dict = None):
        super().__init__()
        self.fields = fields or {}  # 字段名到字段对象的映射
        self.variant_part = variant_part  # 变体部分信息
        
        # 如果没有提供字段，初始化为空字典
        if self.fields is None:
            self.fields = {}
    
    def init_from_type_info(self, type_info: dict):
        """根据类型信息初始化记录对象"""
        # 初始化常规字段
        for field_name, field_type in type_info.get('fields', {}).items():
            self.fields[field_name] = self._create_default_object(field_type)
        
        # 初始化变体部分（如果有）
        variant_info = type_info.get('variant_part')
        if variant_info:
            self.variant_part = {
                'tag_field': variant_info['tag_field'],
                'tag_type': variant_info['tag_type'],
                'variant_cases': variant_info['variant_cases']
            }
            
            # 获取标签字段的当前值
            tag_field_name = variant_info['tag_field']
            tag_field = self.fields.get(tag_field_name)
            
            if tag_field and hasattr(tag_field, 'value'):
                # 根据标签字段的值初始化相应的变体字段
                self._init_variant_fields(tag_field.value)
    
    def _create_default_object(self, type_info: dict) -> Object:
        """根据类型信息创建默认对象"""
        type_name = type_info.get('name', '')
        
        if type_name == "INTEGER":
            return IntegerObject(0)
        elif type_name == "REAL":
            return RealObject(0.0)
        elif type_name == "BOOLEAN":
            return BooleanObject(False)
        elif type_name == "STRING":
            return StringObject("")
        elif type_name == "CHAR":
            return CharObject("")
        elif type_name == "ARRAY":
            return ArrayObject(type_info)
        elif type_name == "ENUM":
            enum_values = type_info.get('values', {})
            if enum_values:
                first_value = list(enum_values.keys())[0]
                return EnumObject(type_info.get('type_name', ''), first_value, 0)
            else:
                return NullObject()
        elif type_name == "RECORD":
            # 创建嵌套记录对象
            nested_record = RecordObject()
            nested_record.init_from_type_info(type_info)
            return nested_record
        else:
            # 其他类型使用空对象
            return NullObject()
    
    def _init_variant_fields(self, tag_value: str):
        """根据标签值初始化变体字段"""
        if not self.variant_part:
            return
        
        # 清除现有的变体字段
        variant_fields = set()
        for case_fields in self.variant_part['variant_cases'].values():
            variant_fields.update(case_fields.keys())
        
        for field_name in variant_fields:
            if field_name in self.fields:
                del self.fields[field_name]
        
        # 添加新的变体字段
        if tag_value in self.variant_part['variant_cases']:
            for field_name, field_type in self.variant_part['variant_cases'][tag_value].items():
                self.fields[field_name] = self._create_default_object(field_type)
    
    def __str__(self):
        fields_str = ", ".join([f"{name}={value}" for name, value in self.fields.items()])
        return f"Record({fields_str})"
    
    def __getitem__(self, field_name: str):
        """获取字段值"""
        return self.fields.get(field_name, NullObject())
    
    def __setitem__(self, field_name: str, value: Object):
        """设置字段值"""
        # 检查字段是否在常规字段或变体字段中
        is_valid_field = field_name in self.fields
        
        if not is_valid_field and self.variant_part:
            # 检查是否是变体字段
            for case_fields in self.variant_part['variant_cases'].values():
                if field_name in case_fields:
                    is_valid_field = True
                    break
        
        if is_valid_field:
            self.fields[field_name] = value
            
            # 如果设置的是标签字段，需要重新初始化变体字段
            if self.variant_part and field_name == self.variant_part['tag_field']:
                if hasattr(value, 'value'):
                    self._init_variant_fields(value.value)
        else:
            raise KeyError(f"Field '{field_name}' not found in record")
```

#### 3.4.2 扩展解释器访问方法
```python
# 在 Interpreter 类中添加以下方法

def visit_RecordType(self, node: RecordType) -> None:
    """访问记录类型定义节点"""
    # 在解释器中，记录类型定义已经在语义分析阶段处理
    pass

def visit_VarDecl(self, node: VarDecl) -> None:
    """访问变量声明节点，不使用RecordTypeSymbol创建RecordObject"""
    # 处理变量类型
    self.visit(node.type_node)
    var_type = self.current_type
    
    # 创建变量符号
    var_name = node.var_node.value
    var_symbol = VarSymbol(var_name, var_type)
    
    # 将变量符号添加到当前作用域
    if self.current_scope is None:
        raise SemanticError(
            error_code=ErrorCode.MISSING_CURRENT_SCOPE,
            token=node.var_node.token,
            message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.var_node.token}",
        )
    
    self.current_scope.insert(var_symbol)
    
    # 在解释器中创建变量对象
    ar = self.call_stack.peek()
    
    # !!! 需要修改， 直接传递 RecordType给 RecordObject
    if hasattr(var_type, 'fields') and hasattr(var_type, 'variant_part'):
        # 创建记录对象，使用类型信息而不是RecordTypeSymbol
        type_info = {
            'fields': {name: {'name': field.type.name} for name, field in var_type.fields.items()},
            'variant_part': None
        }
        
        if var_type.variant_part:
            type_info['variant_part'] = {
                'tag_field': var_type.variant_part.tag_field,
                'tag_type': {'name': var_type.variant_part.tag_type.name},
                'variant_cases': {}
            }
            
            for tag_value, case_fields in var_type.variant_part.variant_cases.items():
                type_info['variant_part']['variant_cases'][tag_value] = {
                    name: {'name': field.type.name} for name, field in case_fields.items()
                }
        
        record_obj = RecordObject()
        record_obj.init_from_type_info(type_info)
        ar[var_name] = record_obj
    elif isinstance(var_type, ArrayTypeSymbol):
        # 创建数组对象
        ar[var_name] = ArrayObject(var_type)
    elif isinstance(var_type, EnumTypeSymbol):
        # 创建枚举对象，使用第一个枚举值
        enum_values = list(var_type.values.keys())
        if enum_values:
            first_value = enum_values[0]
            ar[var_name] = EnumObject(var_type.name, first_value, 0)
        else:
            ar[var_name] = NullObject()
    else:
        # 其他类型使用空对象
        ar[var_name] = NullObject()

def visit_AccessExpression(self, node: AccessExpression) -> Object:
    """访问访问表达式节点，支持嵌套访问如 arr[1].b.c.d[0]"""
    # 从基础表达式开始
    base_obj = self.visit(node.base)
    current_obj = base_obj
    
    # 依次应用每个后缀
    for suffix in node.suffixes:
        if isinstance(suffix, IndexSuffix):
            # 处理数组/字符串索引访问
            index_obj = self.visit(suffix.index)
            index = index_obj.value if isinstance(index_obj, NumberObject) else 0
            
            if isinstance(current_obj, StringObject):
                current_obj = current_obj[index]
            elif isinstance(current_obj, ArrayObject):
                current_obj = current_obj[index]
            else:
                # 不支持索引访问的类型
                return NullObject()
                
        elif isinstance(suffix, MemberSuffix):
            # 处理记录成员访问
            field_name = suffix.member.value
            
            if isinstance(current_obj, RecordObject):
                current_obj = current_obj[field_name]
            else:
                # 不支持成员访问的类型
                return NullObject()
    
    return current_obj if current_obj is not None else NullObject()

def visit_Assign(self, node: Assign) -> None:
    """访问赋值节点，支持对嵌套访问表达式的赋值"""
    # 计算右侧表达式的值
    var_value = self.visit(node.right)
    ar = self.call_stack.peek()
    
    if isinstance(node.left, AccessExpression):
        # 处理访问表达式赋值（如 arr[i] := value 或 record.field := value）
        # 获取基础对象
        base_obj = self.visit(node.left.base)
        current_obj = base_obj
        
        # 处理所有后缀，除了最后一个
        for suffix in node.left.suffixes[:-1]:
            if isinstance(suffix, IndexSuffix):
                # 处理数组/字符串索引访问
                index_obj = self.visit(suffix.index)
                index = index_obj.value if isinstance(index_obj, NumberObject) else 0
                
                if isinstance(current_obj, StringObject):
                    current_obj = current_obj[index]
                elif isinstance(current_obj, ArrayObject):
                    current_obj = current_obj[index]
                else:
                    # 不支持索引访问的类型
                    return
                    
            elif isinstance(suffix, MemberSuffix):
                # 处理记录成员访问
                field_name = suffix.member.value
                
                if isinstance(current_obj, RecordObject):
                    current_obj = current_obj[field_name]
                else:
                    # 不支持成员访问的类型
                    return
        
        # 处理最后一个后缀，进行赋值
        last_suffix = node.left.suffixes[-1]
        if isinstance(last_suffix, IndexSuffix):
            # 处理数组/字符串索引赋值
            index_obj = self.visit(last_suffix.index)
            index = index_obj.value if isinstance(index_obj, NumberObject) else 0
            
            if isinstance(current_obj, StringObject) and isinstance(var_value, (StringObject, CharObject)):
                # 字符串字符赋值
                if isinstance(var_value, StringObject) and len(var_value.value) == 1:
                    current_obj[index] = CharObject(var_value.value)
                elif isinstance(var_value, CharObject):
                    current_obj[index] = var_value
            elif isinstance(current_obj, ArrayObject):
                # 数组元素赋值
                current_obj[index] = var_value
                
        elif isinstance(last_suffix, MemberSuffix):
            # 处理记录成员赋值
            field_name = last_suffix.member.value
            
            if isinstance(current_obj, RecordObject):
                current_obj[field_name] = var_value
                
    elif isinstance(node.left, Var):
        # 处理简单变量赋值
        var_name = node.left.value
        
        # 处理字符串赋值与长度限制
        existing_var = ar.get(var_name)
        if isinstance(existing_var, StringObject) and isinstance(var_value, StringObject):
            if existing_var.limit > 0 and len(var_value.value) > existing_var.limit:
                message = f"Warning: String literal has more characters[{len(var_value.value)}] than short string length[{existing_var.limit}]"
                SpiUtil.print_w(message=message)
                ar[var_name] = StringObject(var_value.value[:existing_var.limit], existing_var.limit)
            else:
                ar[var_name] = StringObject(var_value.value, existing_var.limit)
        elif isinstance(existing_var, CharObject) and isinstance(var_value, StringObject):
            ar[var_name] = CharObject(value=var_value.value)
        else:
            ar[var_name] = var_value
```

## 4. 测试计划

### 4.1 词法/语法分析器测试
```pascal
program TestRecord;
type
    Date = record
        day: 1..31;
        month: 1..12;
        year: integer;
    end;
    
    ShapeKind = (Circle, Rectangle, Triangle);
    
    Shape = record
        x: integer;
        y: integer;
        color: string;
        kind: ShapeKind;
        case kind of
            Circle: (
                radius: real
            );
            Rectangle: (
                width: real;
                height: real
            );
            Triangle: (
                base: real;
                height: real
            );
    end;

var
    d: Date;
    s: Shape;
begin
    d.day := 15;
    d.month := 6;
    d.year := 2023;
    
    s.x := 10;
    s.y := 20;
    s.color := 'red';
    s.kind := Circle;
    s.radius := 5.0;
end.
```

### 4.2 解释器测试
```pascal
program TestRecordAccess;
type
    Person = record
        name: string;
        age: integer;
        address: record
            street: string;
            city: string;
            zip: integer;
        end;
    end;

var
    p: Person;
begin
    p.name := 'John Doe';
    p.age := 30;
    p.address.street := '123 Main St';
    p.address.city := 'Anytown';
    p.address.zip := 12345;
    
    // 测试嵌套访问
    writeln(p.name);               // 输出: John Doe
    writeln(p.address.street);     // 输出: 123 Main St
    writeln(p.address.city);       // 输出: Anytown
    writeln(p.address.zip);        // 输出: 12345
end.
```

## 5. 示例程序

这些示例程序如果是生成新的文件， 然后通过python spi.py filename来调用的话， 可以输出信息也可以调用函数或过程。
但如果是通过 test_interpreter.py 来测试的话， 由于测试代码使用的 call_stack 是 TestCallStack ， 不会执行任何 pop 的操作， 因为不能压入新的函数栈， 
测试的方式是赋值， 最后通过ar[key] 取值的方式来验证是否得到了需要的值

### 5.1 基本记录示例
```pascal
program BasicRecord;
type
    Date = record
        day: 1..31;
        month: 1..12;
        year: integer;
    end;

var
    d: Date;
begin
    d.day := 15;
    d.month := 6;
    d.year := 2023;
    
    writeln('Date: ', d.day, '/', d.month, '/', d.year);
end.
```

### 5.2 变体记录示例
```pascal
program VariantRecord;
type
    ShapeKind = (Circle, Rectangle, Triangle);
    
    Shape = record
        x: integer;
        y: integer;
        color: string;
        kind: ShapeKind;
        case kind of
            Circle: (
                radius: real
            );
            Rectangle: (
                width: real;
                height: real
            );
            Triangle: (
                base: real;
                height: real
            );
    end;

var
    s: Shape;
begin
    s.x := 10;
    s.y := 20;
    s.color := 'red';
    
    // 设置为圆形
    s.kind := Circle;
    s.radius := 5.0;
    writeln('Circle with radius: ', s.radius);
    
    // 设置为矩形
    s.kind := Rectangle;
    s.width := 10.0;
    s.height := 20.0;
    writeln('Rectangle with width: ', s.width, ' and height: ', s.height);
    
    // 设置为三角形
    s.kind := Triangle;
    s.base := 10.0;
    s.height := 15.0;
    writeln('Triangle with base: ', s.base, ' and height: ', s.height);
end.
```

### 5.3 嵌套记录示例
```pascal
program NestedRecord;
type
    Address = record
        street: string;
        city: string;
        zip: integer;
    end;
    
    Person = record
        name: string;
        age: integer;
        address: Address;
    end;

var
    p: Person;
begin
    p.name := 'John Doe';
    p.age := 30;
    p.address.street := '123 Main St';
    p.address.city := 'Anytown';
    p.address.zip := 12345;
    
    writeln('Name: ', p.name);
    writeln('Age: ', p.age);
    writeln('Address: ', p.address.street, ', ', p.address.city, ', ', p.address.zip);
end.
```

## 6. JSON 相关记录类型支持

```pascal
program JsonRecord;
type
    JsonValueKind = (String, Number, Boolean, Null, Object, Array);
    
    JsonValue = record
        kind: JsonValueKind;
        case kind of
            String: (
                str_value: string
            );
            Number: (
                num_value: real
            );
            Boolean: (
                bool_value: boolean
            );
            Null: (
                null_value: integer
            );
            Object: (
                obj_properties: array[1..100] of record
                    name: string;
                    value: JsonValue;
                end
            );
            Array: (
                arr_elements: array[1..100] of JsonValue
            );
    end;

var
    json_obj: JsonValue;
begin
    // 创建一个JSON对象
    json_obj.kind := Object;
    // 设置对象属性...
end.
```

## 7. 实施步骤

1. **扩展词法分析器**：添加 RECORD, CASE, OF 关键字支持
2. **扩展语法分析器**：实现记录类型定义的解析，包括变体部分，使用"case kind of"语法格式
3. **扩展符号系统**：添加记录类型和字段符号类
4. **扩展语义分析器**：实现记录类型的语义检查，特别是变体记录的标签字段验证
5. **扩展解释器**：实现记录对象的创建和访问，支持嵌套访问，不依赖RecordTypeSymbol
6. **编写测试用例**：验证记录类型的各种功能
7. **优化和调试**：修复发现的问题，优化性能

## 8. 风险评估与缓解措施

### 8.1 潜在风险
1. **变体记录的复杂性**：变体记录的处理逻辑较为复杂，可能引入错误
2. **嵌套访问的性能**：深度嵌套的访问可能影响解释器性能
3. **内存管理**：记录对象可能占用较多内存，需要合理管理
4. **不依赖RecordTypeSymbol的实现**：新的实现方式可能引入新的问题

### 8.2 缓解措施
1. **分步实现**：先实现基本记录功能，再添加变体记录支持
2. **单元测试**：为每个功能点编写详细的单元测试
3. **性能优化**：对频繁访问的路径进行优化，如缓存字段访问结果
4. **渐进式重构**：逐步替换RecordTypeSymbol的使用，确保每一步都经过充分测试

## 9. 验收标准

1. **基本记录功能**：能够定义、创建和访问基本记录类型
2. **变体记录功能**：能够定义和使用变体记录，使用"case kind of"语法格式，标签字段必须是枚举类型
3. **嵌套访问**：支持深度嵌套的访问，如 `arr[1].b.c.d[0]`
4. **语义验证**：能够正确验证变体记录的标签字段类型
5. **示例程序**：所有示例程序能够正确运行并产生预期输出
6. **不依赖RecordTypeSymbol**：记录对象的创建和访问不依赖RecordTypeSymbol

## 10. 总结

本计划详细说明了在 Simple Pascal Interpreter 中实现对 Pascal record 类型的支持，包括基本记录和变体记录。通过扩展词法分析器、语法分析器、符号系统和解释器，我们将能够完整地支持 Pascal 记录类型，包括嵌套访问和变体记录的特殊处理。此更新版计划包含四项重要修改：变体记录语法格式改为"case kind of"、更新BNF语法和解析过程、修改RecordObject初始化方式不依赖RecordTypeSymbol、清除解释器中使用RecordTypeSymbol解析的代码。实现过程中将遵循分步实施的原则，确保每个功能点都经过充分测试，最终提供一个稳定、高效的记录类型支持。
```
        