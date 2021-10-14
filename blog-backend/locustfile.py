import time
from locust import HttpUser, task, between


common_headers = { "content-type": "text/plain;charset=utf-8" }


class User(HttpUser):

    wait_time = between(1, 5)

    @task(1)
    def unicode_to_path_1(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/unicode-to-path",
            data = b"123",
            headers = common_headers,
            catch_response = True,
        )

        with request as response:

            check_200(response)

            # TODO: Handle JSON error.
            compiler_msg = response.json()["compilerError"]
            if "TYPE MISMATCH" not in compiler_msg:  
                response.failure("Wrong compiler error message: " + compiler_msg )

    @task(1)
    def unicode_to_path_2(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/unicode-to-path",
            data = "\"/static/noto-emoji/32/emoji_u1f600.png\"",
            headers = common_headers,
            catch_response = True,
        )

        with request as response:

            check_200(response)

            if isinstance(response.json(), dict):
                if response.json().get("compilerError") != None:
                    response.failure(
                        "Code is incorrect but should compiles, but compiler throws error: " + \
                        compiler_error
                    )
                else:
                    response.failure(
                        "Unexpected response. response.json() is dict, but has no \"compilerError\" key."
                    )

            elif isinstance(response.json(), list):
                if len(response.json()) != 3:
                    response.failure("response.json() is of unexpected length " + len(response.json()))
                    return

                numPass = 0
                numFail = 0
                numWrong = 0
                for o in response.json():
                    if o.get("pass") == True:
                        numPass += 1
                    elif o.get("pass") == False and \
                         o.get("expected") == "\"/static/noto-emoji/32/emoji_u1f916.png\"" and \
                         o.get("actual") == "\"/static/noto-emoji/32/emoji_u1f600.png\"":
                        numFail += 1
                    elif o.get("pass") == False and \
                         o.get("expected") == "\"/static/noto-emoji/32/emoji_u2764.png\"" and \
                         o.get("actual") == "\"/static/noto-emoji/32/emoji_u1f600.png\"":
                        numFail += 1
                    else:
                        numWrong +=1

                if numWrong > 0:
                    response.failure("response.json() has invalid objects.")
                else:
                    if not (numPass == 1 and numFail == 2):
                        response.failure("Response has wrong number of pass/fail cases.")

    @task(1)
    def unicode_to_path_3(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/unicode-to-path",
            data = "\"/static/noto-emoji/32/emoji_u\" ++ unicode ++ \".png\"",
            headers = common_headers,
            catch_response = True,
        )

        with request as response:

            check_200(response)

            if isinstance(response.json(), dict):
                if response.json().get("compilerError") != None:
                    response.failure(
                        "Given correct code, compiler throws error: " + compiler_error
                    )
                else:
                    response.failure(
                        "Unexpected response. response.json() is dict, but has no \"compilerError\" key."
                    )

            elif isinstance(response.json(), list):
                if len(response.json()) != 3:
                    response.failure("response.json() is of unexpected length " + len(response.json()))
                    return

                numPass = 0
                for o in response.json():
                    if o.get("pass") == True:
                        numPass += 1

                if numPass != 3:
                    response.failure(
                        "Given correct code, response has wrong number of passed cases: " + \
                        str(numPass)
                    )

    @task(1)
    def render_1(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/render",
            json = {
                "noColonCase": "1",
                "notEmojiCase": "2",
                "isEmojiCase": "3",
            },
            headers = { "content-type": "application/json" },
            catch_response = True,
        )

        with request as response:

            check_200(response)

            if not isinstance(response.json(), dict):
                response.failure("Response is not json dict.")

            compiler_error = response.json().get("compilerError")
            if compiler_error != None:
                if "TYPE MISMATCH" not in compiler_error:
                    response.failure("Incorrect compiler error message: " + compiler_error)
            else:
                response.failure(
                    "Code should not compile, but response json has no \"compilerError\" key."
                )

    @task(1)
    def render_2(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/render",
            json = {
                "noColonCase": "[]",
                "notEmojiCase": "[]",
                "isEmojiCase": "[]",
            },
            headers = { "content-type": "application/json" },
            catch_response = True,
        )

        with request as response:

            check_200(response)

            if not isinstance(response.json(), dict):
                response.failure("Response is not json dict.")

            html = response.json().get("html")
            if html != None:
                # TODO: Number of substring occurrences in the html doesn't indicate 
                #       Number of times they show up after rendered. Research new ways.
                #if not (html.count("No colon in this sentence.") == 1 and \
                #        html.count("One colon : here.") == 1 and \
                #        html.count("One colon at the end:") == 1):
                #    response.failure("Returned html has unexpected number of various substrings.")
                pass
            else:
                resp_json_str = ""
                for k, v in response.json().items():
                    resp_json_str += str(k) + ": " + str(v) + ", "
                response.failure(
                    "Code should have compiled and html should be returned, but response json has no \
                    \"html\" key. response json is: " + resp_json_str
                )

    @task(1)
    def render_3(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/render",
            json = {
                "noColonCase": "[ Text str ]",
                "notEmojiCase": "(replaceEmojis <| String.dropLeft secondColonIndex str)",
                "isEmojiCase":
                    "[ Text <| String.left firstColonIndex str\n, Emoji possibleEmojiName\n]"
            },
            headers = { "content-type": "application/json" },
            catch_response = True,
        )

        with request as response:

            check_200(response)

            check_dict(response)

            html = response.json().get("html")
            if html != None:
                # TODO: Number of substring occurrences in the html doesn't indicate 
                #       Number of times they show up after rendered. Research new ways.
                #if not html.count("No colon in this sentence.") == 2 and \
                #       html.count("One colon : here.") == 2 and \
                #       html.count("One colon at the end:") == 2:
                #    response.failure("Code is correct, but \
                #        returned html has unexpected number of various substrings."
                #    )
                pass
            else:
                resp_json_str = ""
                for k, v in response.json().items():
                    resp_json_str += str(k) + ": " + str(v) + ", "
                response.failure(
                    "Code is correct and html should be returned, but response json has no \
                    \"html\" key. response json is: " + resp_json_str
                )


def check_200(response):
    if response.status_code != 200:
        response.failure("Non 200 status code: " + str(response.status_code))


def check_dict(response):
    if not isinstance(response.json(), dict):
        response.failure("Response is not json dict.")

