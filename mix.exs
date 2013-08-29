defmodule Mambo.Mixfile do
	use Mix.Project

	def project do
		[ app: :mambo,
		  version: "0.2",
		  elixir: "~> 0.10.2-dev",
		  deps: deps ]
	end

	# Configuration for the OTP application
	def application do
		[ applications: [:ssl, :inets],
		  mod: {Mambo, []} ]
	end

	# Returns the list of dependencies in the format:
	# { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
	defp deps do
		[ {:jiffy, [github: "davisp/jiffy"]} ]
	end
end
