# purescript-alexa-template

Clone this repo to get started building an Alexa skill for Purescript

## Prerequisites

1. Node.js (and npm)
2. Purescript, psc-package, and pulp.

```sh
npm install -g purescript psc-package pulp
```

3. The ask-cli Alexa Skills Kit command-line interface.

```sh
npm install -g ask-cli
```

4. Initialize ask-cli with your AWS credentials
```sh
ask init
```

## Get started

1. Clone this repo

```sh
git clone https://github.com/twitchard/purescript-alexa-template
```

2. Build

```sh
npm run build-all
```

3 (optional). You may want to open `.ask/config` and edit deploy_settings.default.merge.skillManifest.apis.custom.endpoint.uri to be the desired name of your lambda function. Otherwise it will be set to `ask-custom-purescript_template-default`.
 

3. Deploy

```sh
npm run deploy-all
```

4. Now go to
https://console.aws.amazon.com/lambda

select your lambda function, and change the 'Handler' to `Main/index.handler`.

5. Test

Go to https://developer.amazon.com/edw/home.html#/skills , log in to your Amazon Developer account, select your newly created skill, click "test" in the side menu, flip the switch to enable the skill for testing.

Then click "Go to Test Simulator" and type "open purescript template" to invoke your skill. Or just say it to Alexa if you have one nearby hooked up to your account.

6. Edit

Tweak the code to your heart's delight!
