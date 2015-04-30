defmodule MazurkaDsl.Mixfile do
  use Mix.Project

  def project do
    [app: :mazurka_dsl,
     version: "0.1.1",
     elixir: "~> 1.0",
     description: "DSL for defining mazurka resources",
     package: package,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    []
  end

  defp package do
    [files: ["src", "mix.exs", "README*"],
     contributors: ["Cameron Bytheway"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/mazurka/mazurka_dsl"}]
  end
end
