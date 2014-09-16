module machine.state;

import std.conv;
import std.bitmanip;

interface Register
{
  @property string name();
  @property byte[] bytes(); 
  @property byte[] bytes(byte[] newValue);
}

class SimpleRegister(T) : Register
{
  string name_;
  T value;
 
  @property string name() { return name_; }
  @property byte[] bytes() { 
    //todo: convert to bytes
    return new byte[0]; 
  }
    //return to!byte[](value); }
  @property byte[] bytes(byte[] newValue) { 
//    value = newValue.read!T();
    //todo: some magic to convert the bytes to our T
    return newValue;
  } 

  this(in string name, in T value) {
    this.name_ = name;
    this.value = value;
  }
   
}

class Memory {
  byte[] data;
  ulong offset;

  this(ulong size, ulong offset =0) 
  { 
    this.offset = offset;
    this.data = new byte[size];
  }
  //overload slice?

}

interface MachineState {
  @property Register[] registers();
  @property Memory[string]  memories();
  
}
