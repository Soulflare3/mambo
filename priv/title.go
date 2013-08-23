package main

import (
	"os"
	"fmt"
	"regexp"
	"strings"
	"net/http"
	"net/http/cookiejar"
	"io/ioutil"
)

func main() {
	req, err := http.NewRequest("GET", os.Args[1], nil)
	if err != nil {
		panic(err)
	}

	jar, err := cookiejar.New(nil)
	if err != nil {
		panic(err)
	}
	c := []*http.Cookie{&http.Cookie{Name: "locale", Value: "en_US", Path: "/", Domain: ".facebook.com"}}
	jar.SetCookies(req.URL, c)

	client := &http.Client{Jar: jar}
	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	ct := strings.Split(resp.Header["Content-Type"][0], ";")[0]
	if ct != "text/html" {
		fmt.Printf("c%s", ct)
	} else {
		cont, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			panic(err)
		}

		r, _ := regexp.Compile(`(?im)<title[^>]*>([^<]+)</title>`)
		if title := r.FindStringSubmatch(string(cont)); title != nil {
			lines := strings.Split(strings.TrimSpace(title), "\n")
			for i, l := range lines {
				lines[i] = strings.TrimSpace(l)
			}

			fmt.Printf("t%s", strings.Join(lines, " "))
		}
	}
}
