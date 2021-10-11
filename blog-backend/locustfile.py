import time
from locust import HttpUser, task, between


common_headers = { "content-type": "text/plain;charset=utf-8" }


class User(HttpUser):

    wait_time = between(1, 5)

    @task
    def unicode_to_path_1(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/unicode-to-path",
            data = b"123",
            headers = common_headers,
            catch_response = True,
        )

        with request as response:
            if response.status_code != 200:
                response.failure("Non 200 status code." )

            # TODO: Handle JSON error.
            compiler_msg = response.json()["compilerError"]
            if "TYPE MISMATCH" not in compiler_msg:  
                response.failure("Wrong compiler error message: " + compiler_msg )

    @task
    def unicode_to_path_2(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/unicode-to-path",
            data = "\"/static/noto-emoji/32/emoji_u1f600.png\"",
            headers = common_headers,
            catch_response = True,
        )

        with request as response:
            if response.status_code != 200:
                response.failure("Non 200 status code." )

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

    @task
    def unicode_to_path_3(self):
        request = self.client.put(
            "/blog-apis/emojis-in-elm/unicode-to-path",
            data = "\"/static/noto-emoji/32/emoji_u\" ++ unicode ++ \".png\"",
            headers = common_headers,
            catch_response = True,
        )

        with request as response:
            if response.status_code != 200:
                response.failure("Non 200 status code." )

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

