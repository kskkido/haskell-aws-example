module Portfolio.Applications.DeployStack.Main
  ( main
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified System.IO as IO
import qualified Stratosphere
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.QQ as Aeson.QQ
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Portfolio.Lib.Aeson.Main as Aeson
import qualified Portfolio.Lib.Either.Main as Either
import qualified Portfolio.Applications.DeployStack.Data.App.Main as App
import qualified Portfolio.Applications.DeployStack.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Applications.DeployStack.Data.Stack.Main as Stack
import qualified Portfolio.Applications.DeployStack.Data.LambdaSource.Main as LambdaSource

main :: IO.IO ()
main = do
  config <- Either.liftFail AppConfig.fromSystem
  App.exec config $ do
    template <- pure
      ( ( Stratosphere.template $ Stratosphere.Resources
            [ ( ( Stratosphere.lambdaFunction
                    ( ( Stratosphere.lambdaFunctionCode ) &
                      ( Stratosphere.lfcS3Bucket ?~
                          ( Stratosphere.Literal $ Text.pack config.lambdaBucketName )
                      ) &
                      ( Stratosphere.lfcS3Key ?~
                          ( Stratosphere.Literal $ Text.pack config.lambdaFilePath )
                      )
                    )
                    ( Stratosphere.Literal "generateStaticSite" )
                    ( Stratosphere.GetAtt "LambdaGenerateStaticSiteExecutionRole" "Arn" )
                    ( Stratosphere.Literal $ Stratosphere.OtherRuntime "provided" )
                ) &
                ( Stratosphere.lfTimeout ?~
                    ( Stratosphere.Literal 120 )
                ) &
                ( Stratosphere.lfEnvironment ?~
                    ( ( Stratosphere.lambdaFunctionEnvironment ) &
                      ( Stratosphere.lfeVariables ?~
                          ( Aeson.KeyMap.fromList
                              [ ( "PUSH_BUCKET_NAME"
                                , Aeson.toJSON $ (Stratosphere.Ref "StaticSiteBucket" :: Stratosphere.Val String)
                                )
                              , ( "DISTRIBUTION_ID"
                                , Aeson.toJSON $ (( Stratosphere.GetAtt "StaticSiteDistribution"  "Id" ):: Stratosphere.Val String)
                                )
                              ]
                          ) <>
                          ( AppConfig.toEnvJson config
                          )
                      )
                    )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSite" )
              )
            , ( ( Stratosphere.lambdaVersion
                  ( Stratosphere.Ref "LambdaGenerateStaticSite" )
                ) &
                ( Stratosphere.lvDescription ?~ Stratosphere.Literal "v3" ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteVersion" )
              )
            , ( ( Stratosphere.lambdaAlias
                  ( Stratosphere.Ref "LambdaGenerateStaticSite" )
                  ( Stratosphere.GetAtt "LambdaGenerateStaticSiteVersion" "Version" )
                  ( Stratosphere.Literal "latest" )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteLatestAlias" )
              )
            , ( ( Stratosphere.lambdaPermission
                  ( Stratosphere.Literal "lambda:InvokeFunction" )
                  ( Stratosphere.GetAtt "LambdaGenerateStaticSite" "Arn" )
                  ( Stratosphere.Literal "apigateway.amazonaws.com" )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteInvokePermission" )
              )
            , ( ( Stratosphere.apiGatewayV2Api
                ) &
                ( Stratosphere.agvapName ?~
                  ( Stratosphere.Literal "LambdaGenerateStaticSiteApi" )
                ) &
                ( Stratosphere.agvapProtocolType ?~
                  ( Stratosphere.Literal "HTTP" )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteApi" )
              )
            , ( ( Stratosphere.apiGatewayV2Route
                  ( Stratosphere.Ref "LambdaGenerateStaticSiteApi" )
                  ( Stratosphere.Literal "POST /build" )
                ) &
                ( Stratosphere.agvrTarget ?~
                  ( Stratosphere.Sub
                    ( "integrations/${IntegrationId}" )
                    ( pure $ Aeson.KeyMap.fromList
                        [ ( "IntegrationId"
                          , Stratosphere.Ref "LambdaGenerateStaticSiteApiIntegration"
                          )
                        ]
                    )
                  )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteApiRoute" )
              )
            , ( ( Stratosphere.apiGatewayV2Integration
                  ( Stratosphere.Ref "LambdaGenerateStaticSiteApi" )
                  ( Stratosphere.Literal "AWS_PROXY" )
                ) &
                ( Stratosphere.agviPayloadFormatVersion ?~
                  ( Stratosphere.Literal "2.0" )
                ) &
                ( Stratosphere.agviIntegrationUri ?~
                  ( Stratosphere.Sub
                    ( "arn:aws:apigateway:${AWS::Region}:lambda:path/2015-03-31/functions/${LambdaArn}/invocations" )
                    ( pure $ Aeson.KeyMap.fromList
                        [ ( "LambdaArn"
                          , Stratosphere.GetAtt "LambdaGenerateStaticSite" "Arn"
                          )
                        ]
                    )
                  )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteApiIntegration" )
              )
            , ( ( Stratosphere.apiGatewayV2Stage
                  ( Stratosphere.Ref "LambdaGenerateStaticSiteApi" )
                  ( Stratosphere.Literal "production" )
                ) &
                ( Stratosphere.agvsAutoDeploy ?~
                  ( Stratosphere.Literal True )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteApiProductionStage" )
              )
            , ( ( Stratosphere.s3Bucket ) &
                ( Stratosphere.sbBucketName ?~
                  ( Stratosphere.Sub
                    ( "${BucketName}-${AWS::Region}-${AWS::AccountId}" )
                    ( pure $ Aeson.KeyMap.fromList
                      [ ( "BucketName"
                        , Stratosphere.Literal $ Text.pack config.staticSiteBucketNamePrefix
                        )
                      ]
                    )
                  )
                ) &
                ( Stratosphere.resource "StaticSiteBucket" )
              )
            , ( ( Stratosphere.s3BucketPolicy
                    ( Stratosphere.Ref "StaticSiteBucket" )
                    ( ( Maybe.fromMaybe mempty $ Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Statement": [
                                {
                                  "Action": "s3:GetObject",
                                  "Effect": "Allow",
                                  "Resource": {
                                    "Fn::Sub": "${StaticSiteBucket.Arn}/*"
                                  },
                                  "Principal": {
                                    "AWS": {
                                      "Fn::Sub": "arn:aws:iam::cloudfront:user/CloudFront Origin Access Identity ${StaticSiteDistributionOriginAccessIdentity}"
                                    }
                                  }
                                }
                              ]
                            }
                          |]
                      )
                    )
                ) &
                ( Stratosphere.resource "StaticSiteBucketPolicy" )
              )
            , ( ( Stratosphere.cloudFrontDistribution
                    ( ( Stratosphere.cloudFrontDistributionDistributionConfig
                          ( Stratosphere.Literal True )
                      ) &
                      ( Stratosphere.cfddcOrigins ?~
                          [ ( ( Stratosphere.cloudFrontDistributionOrigin
                                  ( Stratosphere.GetAtt "StaticSiteBucket" "DomainName" )
                                  ( Stratosphere.Literal "S3Origin" )
                              ) &
                              ( Stratosphere.cfdoS3OriginConfig ?~
                                  ( ( Stratosphere.cloudFrontDistributionS3OriginConfig ) &
                                    ( Stratosphere.cfdsocOriginAccessIdentity ?~
                                        ( Stratosphere.Sub
                                            ( "origin-access-identity/cloudfront/${StaticSiteDistributionOriginAccessIdentity}" )
                                            ( pure $ Aeson.KeyMap.fromList
                                                [ ( "StaticSiteDistributionOriginAccessIdentity"
                                                  , Stratosphere.Ref "StaticSiteDistributionOriginAccessIdentity"
                                                  )
                                                ]
                                            )
                                        )
                                    )
                                  )
                              )
                            )
                          ]
                      ) &
                      ( Stratosphere.cfddcAliases ?~
                          ( Stratosphere.ValList
                              []
                          )
                      ) &
                      ( Stratosphere.cfddcViewerCertificate ?~
                          ( ( Stratosphere.cloudFrontDistributionViewerCertificate ) &
                            ( Stratosphere.cfdvcCloudFrontDefaultCertificate ?~ Stratosphere.Literal True )
                          )
                      ) &
                      ( Stratosphere.cfddcDefaultRootObject ?~ Stratosphere.Literal "en"
                      ) &
                      ( Stratosphere.cfddcDefaultCacheBehavior ?~
                          ( ( Stratosphere.cloudFrontDistributionDefaultCacheBehavior
                                ( Stratosphere.Literal "S3Origin" )
                                ( Stratosphere.Literal "redirect-to-https" )
                            ) &
                            ( Stratosphere.cfddcbCachePolicyId ?~
                                ( Stratosphere.Ref "StaticSiteCachePolicy" )
                            )
                          )
                      )
                    )
                ) &
                ( Stratosphere.resource "StaticSiteDistribution" )
              )
            , ( ( Stratosphere.cloudFrontCloudFrontOriginAccessIdentity
                    ( ( Stratosphere.cloudFrontCloudFrontOriginAccessIdentityCloudFrontOriginAccessIdentityConfig
                          ( Stratosphere.Ref "AWS::StackName" )
                      )
                    )
                ) &
                ( Stratosphere.resource "StaticSiteDistributionOriginAccessIdentity" )
              )
            , ( ( Stratosphere.cloudFrontCachePolicy
                    ( ( Stratosphere.cloudFrontCachePolicyCachePolicyConfig
                          ( Stratosphere.Literal 10.0 )
                          ( Stratosphere.Sub
                              ( "${serviceName}-StaticSiteCachePolicy-${stage}" )
                              ( pure $ Aeson.KeyMap.fromList
                                  [ ( "serviceName"
                                    , Stratosphere.Literal $ Text.pack config.serviceName
                                    )
                                  , ( "stage"
                                    , Stratosphere.Literal $ Text.pack config.stage
                                    )
                                  ]
                              )
                            )
                      ) &
                      ( Stratosphere.cfcpcpcMaxTTL ?~
                          ( Stratosphere.Literal 3600.0 )
                      ) &
                      ( Stratosphere.cfcpcpcDefaultTTL ?~
                          ( Stratosphere.Literal 120.0 )
                      ) &
                      ( Stratosphere.cfcpcpcParametersInCacheKeyAndForwardedToOrigin ?~
                          ( Stratosphere.cloudFrontCachePolicyParametersInCacheKeyAndForwardedToOrigin
                              ( Stratosphere.cloudFrontCachePolicyCookiesConfig
                                  ( Stratosphere.Literal "none" )
                              )
                              ( Stratosphere.Literal False )
                              ( Stratosphere.cloudFrontCachePolicyHeadersConfig
                                  ( Stratosphere.Literal "none" )
                              )
                              ( Stratosphere.cloudFrontCachePolicyQueryStringsConfig
                                  ( Stratosphere.Literal "none" )
                              )
                          )
                      )
                    )
                ) &
                ( Stratosphere.resource "StaticSiteCachePolicy" )
              )
            , ( ( Stratosphere.iamRole
                    ( Maybe.fromMaybe mempty $ Aeson.toObject
                        [Aeson.QQ.aesonQQ|
                          {
                            "Version": "2012-10-17",
                            "Statement": [
                              {
                                "Effect": "Allow",
                                "Principal": {
                                  "Service": "lambda.amazonaws.com"
                                },
                                "Action": "sts:AssumeRole"
                              }
                            ]
                          }
                        |]
                    )
                ) &
                ( Stratosphere.iamrRoleName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-lambda-gss-${stage}-${AWS::Region}" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            , ( "stage"
                              , Stratosphere.Literal $ Text.pack config.stage
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.iamrManagedPolicyArns ?~
                    ( Stratosphere.ValList
                        [ Stratosphere.Ref "AllowLambdaLogToCloudWatchLogPolicy"
                        , Stratosphere.Ref "AllowPutStaticSiteBucketPolicy"
                        , Stratosphere.Ref "AllowStaticSiteInvalidationPolicy"
                        ]
                    )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteExecutionRole" )
              )
            , ( ( Stratosphere.logsLogGroup ) &
                ( Stratosphere.llgLogGroupName ?~
                    ( Stratosphere.Sub
                        ( "/aws/lambda/${lambda}" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "lambda"
                              , Stratosphere.Ref "LambdaGenerateStaticSite"
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "LambdaGenerateStaticSiteLogGroup" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ do
                        Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Version": "2012-10-17",
                              "Statement": [
                                {
                                  "Effect": "Allow",
                                  "Action": "logs:CreateLogStream",
                                  "Resource": "*"
                                },
                                {
                                  "Effect": "Allow",
                                  "Action": "logs:PutLogEvents",
                                  "Resource": "*"
                                }
                              ]
                            }
                          |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-AllowLambdaLogToCloudWatchLogPolicy-${stage}-${AWS::Region}" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            , ( "stage"
                              , Stratosphere.Literal $ Text.pack config.stage
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "AllowLambdaLogToCloudWatchLogPolicy" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ Aeson.toObject
                        [Aeson.QQ.aesonQQ|
                          {
                            "Version": "2012-10-17",
                            "Statement": [
                              {
                                "Effect": "Allow",
                                "Action": [
                                  "s3:GetBucketLocation",
                                  "s3:GetObject",
                                  "s3:PutObject",
                                  "s3:ListBucket",
                                  "s3:DeleteObject"
                                ],
                                "Resource": [
                                  {
                                    "Fn::GetAtt": "StaticSiteBucket.Arn"
                                  },
                                  {
                                    "Fn::Sub": "${StaticSiteBucket.Arn}/*"
                                  }
                                ]
                              }
                            ]
                          }
                        |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-AllowPutStaticSiteBucket-${stage}-${AWS::Region}" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            , ( "stage"
                              , Stratosphere.Literal $ Text.pack config.stage
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "AllowPutStaticSiteBucketPolicy" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ Aeson.toObject
                        [Aeson.QQ.aesonQQ|
                          {
                            "Version": "2012-10-17",
                            "Statement": [
                              {
                                "Effect": "Allow",
                                "Action": [
                                  "cloudfront:UpdateDistribution",
                                  "cloudfront:CreateInvalidation"
                                ],
                                "Resource": [
                                  {
                                    "Fn::Sub": "arn:aws:cloudfront::${AWS::AccountId}:distribution/${StaticSiteDistribution.Id}"
                                  }
                                ]
                              }
                            ]
                          }
                        |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-AllowStaticSiteInvalidation-${stage}-${AWS::Region}" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            , ( "stage"
                              , Stratosphere.Literal $ Text.pack config.stage
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "AllowStaticSiteInvalidationPolicy" )
              )
            ]
        ) &
        ( Stratosphere.templateDescription ?~ "DeployStack"
        )
      )
    LambdaSource.upload
    Stack.fromTemplate template
    liftIO $ ByteString.Lazy.Char8.putStrLn
      ( Stratosphere.encodeTemplate template )

