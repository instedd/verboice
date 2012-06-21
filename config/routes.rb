# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

Verboice::Application.routes.draw do

  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts

  # Register both shallow and deep routes:
  # - Shallow routes allow for easier path helper methods, such as contact_recorded_audios(@contact) instead of project_contact_recorded_audios(@project, @contact)
  # - Deep routes ensure that form_for directives work as expected, so form_for([@project, @contact]) works no matter it is a creation or an update
  [true, false].each do |shallow|
    resources :projects, :shallow => shallow do
      member do
        post :enqueue_call
      end

      resources :call_flows, except: [:new, :edit] do
        member do
          get :edit_workflow, path: :designer
          put :update_workflow, path: :update_flow
          get :play_recording
          post :save_recording
          post :import
          get :export
          get :download_results
        end
      end

      resources :external_services, except: [:show, :new, :edit] do
        member do
          put :update_manifest
        end
      end

      resources :schedules

      resources :contacts do
        resources :persisted_variables
        resources :recorded_audios, only: [:index, :show, :delete]
      end

    end
  end

  resources :call_logs, path: :calls do
    member do
      get :progress
      get 'results/:key', :action => :play_result, :as => 'result'
      get :download_details
    end
    collection do
      get :queued
      get :download
    end
  end

  match "api/call" => "api#call", :as => :api_call
  match "api/call/:id/state" => "api#call_state"
  match 'api/call/:id/redirect' => 'api#call_redirect'

  get "api/channels" => "api_channels#list"
  post  "api/channels" => "api_channels#create"
  delete "api/channels/:name" => "api_channels#destroy"

  get "api/schedules" => "api_schedules#index"
  get "api/schedules/:name" => "api_schedules#show"
  post "api/schedules" => "api_schedules#create"
  put "api/schedules/:name" => "api_schedules#update"
  delete "api/schedules/:name" => "api_schedules#destroy"

  get 'api/logs/:call_id' => "api_logs#list"

  get 'oauth/google' => 'oauth#google', :as => 'google_oauth'
  match 'oauth/google/callback' => 'oauth#google_callback', :as => 'google_callback_oauth'

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('/')
end
