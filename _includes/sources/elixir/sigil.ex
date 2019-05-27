#+BEGIN_FOLD Tests do
  ExUnit.start()
#+END_FOLD end

defmodule Sigils do
  #+BEGIN_SOLUTION
  def sigil_u(string, []) do
    String.upcase(string)
  end
  #+END_SOLUTION
end

#+BEGIN_FOLD Tests do
  defmodule SigilsTest do
    use ExUnit.Case

    import Sigils

    test "" do
      assert ~u/Hello World!/ == "HELLO WORLD!"
    end
  end
#+END_FOLD end
