program VariantRecords;

{
  演示变体记录类型
}

type
  TShapeType = (Circle, Rectangle);
  
  TShape = record
    shapeType: TShapeType;
    case TShapeType of
      Circle: (radius: Real);
      Rectangle: (width, height: Real);
  end;

var
  shape1, shape2: TShape;

begin
  // 创建圆形
  shape1.shapeType := Circle;
  shape1.radius := 5.0;
  
  // 创建矩形
  shape2.shapeType := Rectangle;
  shape2.width := 10.0;
  shape2.height := 3.0;
  
  // 输出形状信息
  case shape1.shapeType of
    Circle: writeln('Circle with radius: ', shape1.radius:0:2);
    Rectangle: writeln('Rectangle with width: ', shape1.width:0:2, ' and height: ', shape1.height:0:2);
  end;
  
  case shape2.shapeType of
    Circle: writeln('Circle with radius: ', shape2.radius:0:2);
    Rectangle: writeln('Rectangle with width: ', shape2.width:0:2, ' and height: ', shape2.height:0:2);
  end;
end.