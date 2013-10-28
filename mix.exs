defmodule Mambo.Mixfile do
  use Mix.Project

  def project do
    [
      app: :mambo,
      version: "0.3",
      elixir: "~> 0.10.4-dev",
      name: "Mambo",
      source_url: "https://github.com/mrshankly/mambo",
      homepage_url: "https://github.com/mrshankly/mambo",
      deps: deps
    ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:hackney],
      mod: {Mambo, []} ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [{:jsx, github: "talentdeficit/jsx"},
     {:hackney, github: "benoitc/hackney"}]
  end
end
