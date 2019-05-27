#+BEGIN_FOLD Tests {
ExUnit.start()
#+END_FOLD }

defmodule Sigils do
  #+BEGIN_SOLUTION
  def sigil_u(string, []) do
    String.upcase(string)
  end
  #+END_SOLUTION
end

#+BEGIN_FOLD Tests {
defmodule SigilsTest do
  use ExUnit.Case

  import Sigils

  test "" do
    assert ~u/Hello World!/ == "HELLO WORLD!"
  end
end
#+END_FOLD }
