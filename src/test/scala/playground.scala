
/**
  * @author Marcin Mościcki
  */
class playground {
	trait Assembler {
		type T
	}

	type AssemblerT[X] = Assembler { type T = X }

	class Values[A <: AssemblerT[_]] {
		def get :A#T = ???
//		def value[N](a :AssemblerT[N]) :N = component(a).get
		def component[N](a :AssemblerT[N]) :Values[a.type] = new Values[a.type]
	}
}
